/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** 
 * Asynchronous and abortable rescaling algorithms working on
 * ImageData objects. Web workers are not used because of the data
 * copies they entail. The channels of images are assumed to be in
 * linear space. They are unlikely to be in, if gamma correct
 * resampling is needed, sRGB_to_linear() can be used on the source
 * image and linear_to_sRGB() can be used on the destination image
 * once resampled.
 */

/* A few tips on the code below. 

   Fast JavaScript truncation
   --------------------------

   If x is a *positive* float. 

   - x | 0          is floor(x)
   - (x + 0.5) | 0  is round(x)
   - x << n         is floor(x) * 2^n
   - (x - (x | 0))  is frac(x)

   Coordinate systems
   ------------------

   The image's continous coordinate system is mapped to its discrete,
   one via floor(), which effectively puts the samples at half
   integers (see Heckbert. What are the coordinates of a pixel? In
   A. S. Glassner (Ed.), Graphics Gems I, 1990, pp. 246–48.)
   
     Discrete   0   1   2
                *   *   *
              |---|---|---|-- ...
   Continuous 0   1   2   3

   That is if c and d are respectively the continuous and discrete
   coordinate of a sample we have c = d + 0.5 and d = c | 0. 

   Let (sw, sh) be the dimension of the source image and (dw, dh)
   those of the destination one. A sample (x,y) ∈ [0;dw - 1]x[0;dh -
   1] in the destination discrete coordinate system is mapped to the
   source's continous space with :
   
   xs(x) = (sw / dw) * (x + 0.5)
   ys(y) = (sh / dh) * (y + 0.5)

   Binary pixel mixing resampling
   ------------------------------
   
   In pixel mixing we map the square around each destination sample to
   a square in src and compute the average of the samples that fall
   inside this square. In src we access only the red channel because
   we assume a binary image.
*/

var Rescale = function () 
{
  /* Time sharing */

  /** @const */ var yield_time = 100;
  function now () { return new Date().getTime (); }
  function suspend (t) { return (now () - t) > yield_time; }

  /* Conversion to linear color space and back. */
  
  var lut_sRGB_to_linear = [];
  var lut_linear_to_sRGB = [];
  
  function make_luts ()
  {
    function sRGB_to_linear (s)
    {
      if (s <= 0.04045) return s / 12.92;
      else return Math.pow ((s + 0.055) / 1.055, 2.4); 
    }
    
    function linear_to_sRGB (s)
    {
      if (s <= 0.0031308) return s * 12.92;
      else return 1.055 * Math.pow (s, 1 / 2.4) - 0.055;
    }
    
    for (var i = 0; i < 256; i++) 
    {
      lut_sRGB_to_linear[i] = sRGB_to_linear (i / 255) * 255 + 0.5 | 0; 
      lut_linear_to_sRGB[i] = linear_to_sRGB (i / 255) * 255 + 0.5 | 0; 
    }
  }

  function color_convert (table, img, abort, k)
  {
    if (table.length === 0) make_luts (); 
    function loop (img, i, abort, k)
    {
      var sample_count = (img.width * img.height) << 2;
      var w = img.width;
      var p = img.data; 
      var t = now (); 
      while (i < sample_count && !abort () && !suspend(t)) 
      { i++; if (i % 4 !== 3) {} else { p[i] = table[p[i]]; } }
      if (i === sample_count || abort ()) k (abort, img); 
      else 
      {
        Debug.log ("Color space suspend"); 
        window.setTimeout (loop, 0, img, i, abort, k); 
      }
    }
    window.setTimeout (loop, 0, img, -1, abort, k); 
  }

  function sRGB_to_linear (img, abort, k) 
  { color_convert (lut_sRGB_to_linear, img, abort, k); }; 

  function linear_to_sRGB (img, abort, k) 
  { color_convert (lut_linear_to_sRGB, img, abort, k); }; 

  /** @ignore Type for specifying filters @enum{number} */
  var Filter = { Box : 0, 
                 Linear : 1, 
                 B_spline : 2, 
                 Catmull_rom : 3, 
                 Lanczos_3 : 4, 
                 Binary_pixel_mixing : 5, 
                 Binary_subpixel_mixing3 : 6, 
                 Binary_subpixel_mixing5 : 7 }

  /* Generic resample function by separable convolution kernel. */
  function resample (src, dst, kernel, kw, abort, k)
  {    
    function loop (src, dst, kernel, kw, y, di, abort, k)
    {
      var sp = src.data; var sw = src.width; var sh = src.height;
      var dp = dst.data; var dw = dst.width; var dh = dst.height; 
      var xmax = sw - 1; var ymax = sh - 1;
      var xratio = sw / dw; 
      var yratio = sh / dh;
      var t = now ();
      
      while (y < dh && !abort () && !suspend (t))
      {
        var ys = (y + 0.5) * yratio - 0.5;
        var yoffset = []; var yweight = []; 
        for (var j = 0; j < kw; j++)
        {
          var v = (ys | 0) - ((kw - 1) / 2 | 0) + j;
          var row = (v < 0) ? 0 : (v > ymax) ? ymax : v;
          yoffset[j] = row * sw;
          yweight[j] = kernel (v - ys);
        }

        for (var x = 0; x < dw; x++)
        {
          var xs = (x + 0.5) * xratio - 0.5;
          var xoffset = []; var xweight = [];
          for (var i = 0; i < kw; i++)
          {
            var u = (xs | 0) - ((kw - 1) / 2 | 0) + i; 
            var col = (u < 0) ? 0 : (u > xmax) ? xmax : u; 
            xoffset[i] = col;
            xweight[i] = kernel (u - xs);
          }
          
          for (var c = 0; c < 4; c++)
          {
            var ynorm = 0; 
            var ysum = 0;
            for (var j = 0; j < kw; j++)
            {
              var xnorm = 0; 
              var xsum = 0;
              for (var i = 0; i < kw; i++)
              {
                var index = ((yoffset[j] + xoffset[i]) << 2) + c;
                xsum = xsum + sp[index] * xweight[i]; 
                xnorm += xweight[i];
              }
              ysum = ysum + (xsum / xnorm) * yweight[j];
              ynorm += yweight[j]; 
            }
            dp[++di] = (ysum / ynorm) | 0;
          }
        }
        y++
      }
      if (y === dh || abort ()) k (abort, dst); 
      else 
      {
        Debug.log ("Convolve suspend"); 
        window.setTimeout (loop, 0, src, dst, kernel, kw, y, di, abort, k);
      }
    }
    window.setTimeout (loop, 0, src, dst, kernel, kw, 0, -1, abort, k);
  }

  function box (src, dst, abort, k)
  {
    function box (t) { return (-0.5 <= t && t < 0.5) ? 1 : 0; }
    resample (src, dst, box, 2, abort, k); 
  }
  
  function linear (src, dst, abort, k)
  {
    function linear (t)
    {
      if (t < 0) t = -t; 
      if (t < 1.0) return 1 - t; 
      else return 0;
    }
    resample (src, dst, linear, 2, abort, k);
  }

  function b_spline (src, dst, abort, k) 
  {
    function b_spline (t) /* (B-spline) */
    {
      if (t < 0) t = -t; 
      if (t < 1) return (t * (3 * t * t - 6 * t) + 4) / 6;
      if (t < 2) return (t * (- t * t + 6 * t - 12) + 8) / 6; 
      else return 0;
    }
    resample (src, dst, b_spline, 4, abort, k); 
  }

  function catmull_rom (src, dst, abort, k) 
  {
    function catmull_rom (t) 
    {
      if (t < 0) t = -t;
      if (t < 1) return (t * (3 * t * t - 5 * t) + 2) * 0.5;
      if (t < 2) return (t * (- t * t + 5 * t - 8) + 4) * 0.5; 
      else return 0;
    }
    resample (src, dst, catmull_rom, 4, abort, k);
  }
  
  function lanczos_3 (src, dst, abort, k) 
  {
    function lanczos_3 (t)
    {
      var abs = (t < 0) ? -t : t; 
      if (t === 0) return 1; 
      else if (abs < 3) 
        return Math.sin(Math.PI * (t / 3)) * Math.sin(Math.PI * t) / 
          (Math.PI * Math.PI * t * t);
      else return 0;
    }
    resample (src, dst, lanczos_3, 6, abort, k); 
  }

  function binary_pixel_mixing (src, dst, abort, k) 
  {
    function loop (src, dst, y, di, abort, k)
    {
      var sp = src.data; var sw = src.width; var sh = src.height;
      var dp = dst.data; var dw = dst.width; var dh = dst.height; 
      var xmax = sw - 1; var ymax = sh - 1;
      var xratio = sw / dw; 
      var yratio = sh / dh;
      var t = now ();      
      window.console.time("Binary pixel mixing"); 
      while (y < dh && !abort () && !suspend (t))
      {
        var y0 = y * yratio | 0; 
        var y1 = (y + 1) * yratio | 0; if (y1 > ymax) y1 = ymax; 
        for (var x = 0; x < dw; x++)
        {
          var x0 = x * xratio | 0; 
          var x1 = (x + 1) * xratio | 0; if (x1 > xmax) x1 = xmax;          
          var accum = 0; 
          var count = 0;
          for (var my = y0; my <= y1; my++)
          {
            var offset = my * sw;
            for (var mx = x0; mx <= x1; mx++)
            { accum += sp[((offset + mx) << 2)]; count++; }
          }
          accum = accum / count & 0xFF;
          dp[++di] = dp[++di] = dp[++di] = accum; /* RGB */
          dp[++di] = 255; /* A */
        }
        y++;
      }
      window.console.timeEnd("Binary pixel mixing"); 
      if (y === dh || abort ()) k (abort, dst); 
      else 
      {
        Debug.log ("Binary pixel mixing suspend");
        window.setTimeout (loop, 0, src, dst, y, di, abort, k);
      }
    }
    window.setTimeout (loop, 0, src, dst, 0, -1, abort, k)
  }

  function binary_subpixel_mixing3 (src, dst, abort, k) 
  {
    function loop (src, dst, y, di, abort, k)
    {
      var sp = src.data; var sw = src.width; var sh = src.height;
      var dp = dst.data; var dw = dst.width; var dh = dst.height; 
      var xmax = sw - 1; var ymax = sh - 1;
      var dimax = dw * dh << 2;
      var xratio = sw / (dw * 3) /* each pixel has 3 subpixels */; 
      var yratio = sh / dh;
      var t = now ();      
      window.console.time("Binary subpixel 3-tap"); 
      while (y < dh && !abort () && !suspend (t))
      {
        var y0 = y * yratio | 0; 
        var y1 = (y + 1) * yratio | 0; if (y1 > ymax) y1 = ymax; 
        for (var x = 0; x < dw * 3; x++)
        {
          var x0 = x * xratio | 0; 
          var x1 = (x + 1) * xratio | 0; if (x1 > xmax) x1 = xmax;          
          var accum = 0; 
          var count = 0; 
          for (var my = y0; my <= y1; my++)
          {
            var offset = my * sw;
            for (var mx = x0; mx <= x1; mx++) 
            { accum += sp[(offset + mx) << 2]; count++; }
          }
          accum = accum / count;
          di++; if (di % 4 == 3) dp[di++] = 255; /* A */

          /* Distribute accum on 2 neighbouring subpixels with 
             FIR (1/3, 1/3, 1/3) */
          var channel = x % 3;
          var p0 = di - 1 - ((1 << channel) & 0x1) /* sub 1 for R */;  
          var p1 = di; 
          var p2 = di + 1 + ((4 >> channel) & 0x1) /* add 1 for B */; 
          if (p0 < 0) p0 = di; 
          if (p2 > dimax) p2 = di;
          var waccum = accum / 3 & 0xFF;
          dp[p0] += waccum; 
          dp[p1] += waccum;
          dp[p2] += waccum; 
        }
        dp[++di] = 255; /* last A */
        y++;
      }
      window.console.timeEnd("Binary subpixel 3-tap"); 
      if (y === dh || abort ()) k (abort, dst); 
      else 
      {
        Debug.log ("Binary subpixel 3-tap suspend"); 
        window.setTimeout (loop, 0, src, dst, y, di, abort, k);
      }
    }
    window.setTimeout (loop, 0, src, dst, 0, -1, abort, k);
  }

  function binary_subpixel_mixing5 (src, dst, abort, k) 
  {
    function loop (src, dst, y, di, abort, k)
    {
      var sp = src.data; var sw = src.width; var sh = src.height;
      var dp = dst.data; var dw = dst.width; var dh = dst.height; 
      var dimax = dw * dh << 2;
      var xmax = sw - 1; var ymax = sh - 1;
      var xratio = sw / (dw * 3); 
      var yratio = sh / dh;
      var t = now ();      
      window.console.time("Binary subpixel 5-tap"); 
      while (y < dh && !abort () && !suspend (t))
      {
        var y0 = y * yratio | 0; 
        var y1 = (y + 1) * yratio | 0; if (y1 > ymax) y1 = ymax; 
        for (var x = 0; x < dw * 3; x++)
        {
          var x0 = x * xratio | 0; 
          var x1 = (x + 1) * xratio | 0; if (x1 > xmax) x1 = xmax;          
          var accum = 0; 
          var count = 0; 
          for (var my = y0; my <= y1; my++)
          {
            var offset = my * sw;
            for (var mx = x0; mx <= x1; mx++)
            { accum += sp[(offset + mx) << 2]; count++; }
          }
          accum = accum / count;
          di++; if (di % 4 == 3) dp[di++] = 255; /* A */

          /* Distribute accum on 4 neighbouring subpixels with 
             FIR (1/16, 4/16, 7/3, 4/16, 1/6) */
          var channel = x % 3; 
          var p0 = di - 3 + ((4 >> channel) & 0x1) /* add 1 for B */;  
          var p1 = di - 1 - ((1 << channel) & 0x1) /* sub 1 for R */;;
          var p2 = di; 
          var p3 = di + 1 + ((4 >> channel) & 0x1) /* add 1 for B */; 
          var p4 = di + 3 - ((1 << channel) & 0x1) /* sub 1 for R */;
          if (p0 < 0) { p0 = p1 = di; }
          if (p4 > dimax) { p3 = p2 = di; }
          dp[p0] += accum / 16 & 0xFF; 
          dp[p1] += (4 * accum) / 16 & 0xFF;
          dp[p2] += (7 * accum) / 16 & 0xFF; 
          dp[p3] += (4 * accum) / 16 & 0xFF;
          dp[p4] += accum / 16 & 0xFF; ; 
        }
        dp[++di] = 255; /* last A */
        y++;
      }
      window.console.timeEnd("Binary subpixel 5-tap"); 
      if (y === dh || abort ()) k (abort, dst); 
      else 
      {
        Debug.log ("Binary subpixel 5-tap suspend"); 
        window.setTimeout (loop, 0, src, dst, y, di, abort, k);
      }
    }
    window.setTimeout (loop, 0, src, dst, 0, -1, abort, k);
  }

  function image (filter, src, dst, abort, k)
  {
    var f = linear;
    switch (filter)
    {
    case Filter.Box: f = box; break;
    case Filter.Linear: f = linear; break;
    case Filter.B_spline: f = b_spline; break; 
    case Filter.Catmull_rom: f = catmull_rom; break; 
    case Filter.Lanczos_3: f = lanczos_3; break;
    case Filter.Binary_pixel_mixing: f = binary_pixel_mixing; break;
    case Filter.Binary_subpixel_mixing3: f = binary_subpixel_mixing3; break;
    case Filter.Binary_subpixel_mixing5: f = binary_subpixel_mixing5; break;
    }
    f (src, dst, abort, k);
  }

  return { 
    /** `sRGB_to_linear (img, abort, k)` transforms the sRGB samples
        of `img` to a linear RGB color space. `k (abort, img)` is
        called whenever the computation is finished. `abort` is called
        periodically and can return `true` if the computation needs to
        be aborted. */
    sRGB_to_linear : sRGB_to_linear,

    /** `linear_to_sRGB (img, abort, k)` transforms the linear RGB
        samples of `img` to sRGB. `k (abort, img)` is called whenever
        the computation is finished. `abort` is called periodically and can 
        return `true` if the computation needs to be aborted. */
    linear_to_sRGB : linear_to_sRGB,

    /** The type for resample filters. */
    Filter : Filter,
    
    /** `image (filter, src, dst, abort, k)` resamples `src` into `dst` 
        according to `filter`. `k (abort, dst)` is called whenever the 
        computation is finished. `abort` is called periodically and can 
        return `true` if the computation needs to be aborted. */
    image : image
  };
} ();
