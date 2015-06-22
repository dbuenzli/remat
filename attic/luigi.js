/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** @define {boolean} @ignore */
var DEBUG = true;

/** Debugging tools. */

var Debug = function () 
{
  var console = null; 
  var logf = null;
  switch (typeof window.console.log)
  {
  case "function": console = window.console; logf = window.console.log; break;
  case "object": // IE
    console = null;
    if (Function.prototype.bind) 
      logf = Function.prototype.bind.call(window.console.log, window.console);
    break;
  };
  var log =
    (DEBUG && logf) ?
    function () { logf.apply(console, arguments); } : function () {}; 
  
  return { 
    /** `log (m)` logs `m` in the console of browsers that support it. */
    log : log 
  };  
} ();
/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/



/** Module for the metadata of documents. */

var Doc = function () 
{

  /** @ignore Type for specifing scales @enum {number}   */
  var Scale = { Fit : 0, Width : 1, Real : 2, Custom : 3 }
  
  /** @ignore @typedef {{ name: string, keys: Array.<string> }} */
  var Index = { name : "", keys : [] }

  /** 
   * The type for documents. [name] is the document's name (if any). 
   * [indexes] is the collections to which the document belongs to and
   * [pages] its pages.  
  */
  function doc (name, indexes, views)
  { return { name : name, indexes : indexes, views : views }; }
  
  /** The type for collection indexes. The collection's [name], 
      and the index [keys] in the collection. */
  function index (name, keys) { return { name : name, keys : keys } }

  /** The type for pages. [label] is the page's number (must en a
      string, can be null). 
      [ppcm_x] and [ppcm_y] are the pixel per centimeters in
      each direction. [image_uri] is the uri of the image. [ocr_uri]
      is OCR data for the image. [ocr_index] is integer that indicates
      the page number number in the OCR data coresponding to the page. */
  function page (label, ppcm_x, ppcm_y, image_uri, ocr_uri, ocr_index)
  { 
    return { 
      label : label, ppcm_x : ppcm_x, ppcm_y : ppcm_y, image_uri : image_uri,
      ocr_uri : ocr_uri, ocr_index : ocr_index };
  }
  
  
  /* Temporary placeholder */
  function placeholder (uri)
  {
  }
  
  function of_uri (u) 
  { 
    // TODO temporary placeholder
    function view (p) { return page 
                        (p, 118, 118, 
                         "/luigi/images/lra_1925_04_25_000" + p + ".png",
                         "", 
                         Number(p) - 1); }; 

    var d = doc 
    ("", [index ("Le Réveil gnarchiste", ["1925", "04", "25"])],
     [view ("1"), view ("2"), view ("3"), view ("4")]); 
    return d;
  }
  
  function doc_uri (d)
  {
    return "Le_Réveil_anarchiste/1925/04/25"; // TODO
  }


  function view_uri (d, v)
  {
    return doc_uri (d) + "/v/" + (v + 1).toString(); // TODO
  }
    
  function view_image_uri (d, v) 
  {
    return d.views[v].image_uri; 
  }
  
  return {
    Scale : Scale,
    /** 
     * `of_uri u` returns a document from the uri `u`. 
     */
    of_uri : of_uri,

    /** 
     * `doc_uri d v` returns the uri for the view `v` of `d`. 
     * @sig doc -> int -> string 
     */
    doc_uri : doc_uri,
    view_uri : view_uri,
    view_image_uri : view_image_uri 
  };
} ();

/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** Essential javascript additions. */

var J = function () 
{
  function assert (b) 
  { 
    function Assert_failure /** @constructor @ignore */ () {};
    if (!b) throw new Assert_failure (); 
  }
  
  function map (f, a) 
  {
    var r = new Array (a.length); 
    for (var i = 0; i < a.length; i++) r[i] = f (a[i]);
    return r;
  }

  function mapi (f, a) 
  {
    var r = new Array (a.length); 
    for (var i = 0; i < a.length; i++) r[i] = f (i, a[i]);
    return r;
  }
  
  function iter (f, a) { for (var i = 0; i < a.length; i++) f (a[i]); }
  function iteri (f, a) { for (var i = 0; i < a.length; i++) f (i, a[i]); }
  function fold (f, acc, a) 
  { 
    for (var i = 0; i < a.length; i++) acc = f (acc, a[i]); return acc; 
  }
  
  function app (f)
  {
    var slice = Array.prototype.slice;
    var args = slice.call(arguments, 1);
    return function () 
    { return f.apply(null, args.concat(slice.apply(arguments))); };
  }
  
  function clone (o)   
  { 
    function O /** @constructor @ignore */ () {}; 
    O.prototype = o; return new O (); 
  }
  
  function $ (id) { return document.getElementById(id); }
  function ev (el, e, f) { el.addEventListener(e, f, false); }

  function txt (s) { return document.createTextNode (s); }
  function dom (name) 
  {
    function set_style_attr (n, a, v) { n.style[a] = v; }
    function set_attr (n, a, v)
    {
      if (!v) return; 
      switch (a)  
      {
      case "class": n.className = v; break
      case "checked": n.defaultChecked = v; break;
      case "for" : n.htmlFor = v; break;
      case "selectedIndex": n.selectedIndex = v; /* why ? */ break;
      case "style" : 
        for (var c in v) if (v.hasOwnProperty (c)) set_style_attr (n, c, v[c]);
        break;
      default: n.setAttribute (a, v); break;
      }
    }
    var n = document.createElement (name);
    var atts = null;
    for (var i = 1; i < arguments.length; i++)
    {
      var c = arguments[i];
      if (typeof c === "string")
      { 
        n.appendChild (document.createTextNode(c)); 
      } 
      else if (c instanceof Array)
      { 
        for (var j = 0; j < c.length; j++) n.appendChild (c[j]); 
      }
      else if (i === 1 && c instanceof Object && !(c.nodeType > 0))  /* atts */
      { 
        atts = c; 
      }
      else
      { n.appendChild(c); }
    }

    if (atts) 
      for (var a in atts) if (atts.hasOwnProperty (a)) 
        set_attr (n, a, atts[a]); 
    return n;
  }

  function set_css_width (e, w) { e.style["width"] = w + "px"; return e; }
  function set_css_height (e, h) { e.style["height"] = h + "px"; return e; }
  function set_css_dim (e, w, h) 
  { e.style["width"] = w + "px"; e.style["height"] = h + "px"; return e; }


  function classify (e, c, is_c)
  {
    var re = new RegExp("(.*)(?:^|\\s)" + c + "(?:\\s|$)(.*)");
    var r = re.exec(e.className);
    if (r)
    { if (!is_c) e.className = r[1] + " " + r[2] /* rem. */ ; return true;  }
    else 
    { if (is_c) e.className = e.className + " " + c /* add */; return false; }
  }
  
  return {
    /**
     * `assert (b)` raises an exception if `b` is `false`. 
     *  bool -> unit
     */
    assert : assert,

    /** 
     * `map (f, a)` is an array `b` with `b[i] == f (b[i])`. 
     * - ('a -> 'b) -> 'a array -> 'b array 
     */
    map : map ,

    /** 
     * `map (f, a)` is an array `b` with `b[i] == f (i, b[i])`. 
     * - (int -> 'a -> 'b) -> 'a array -> 'b array 
     */
    mapi : mapi ,

    /** 
     * `iter (f, a)` is `f (a[0]); f (a[1]); ... f (a[a.length - 1]);` 
     *     ('a -> unit) -> 'a array -> unit
     */  
    iter : iter,

    /** 
     * `iteri (f, a)` is `f (0, a[0]); f (1, a[1]); ... 
     *   f (a.length - 1, a[a.length - 1]);` 
     *     (int -> 'a -> unit) -> 'a array -> unit
     */  
    iteri : iteri,

    /** 
     * `fold (f, acc, a)` is 
     * `f (... f (f (acc, a[0]), [1]) ..., a[a.length -1 ])` 
     *     ('a -> 'b -> 'a) -> 'b array -> 'a
     */
    fold : fold,
    
    /** 
     * `app (f, a, ...)` is the function `f (a, ...)`. 
     *     ('a -> 'b -> 'c) -> 'a -> ('b -> 'b)
     */
    app : app,
    
    /** `clone o` clones the object `o`. */ 
    clone : clone,
    
    /** 
     * `$(id)` is `document.getElementById(id)`. 
     * @sig string -> Node ? 
     */
    $ : $,

    /** `ev (el, e, f)` is `el.addEventListener(e, f, false)`. */
    ev : ev,

    /** `txt (t)` is a text node with text `t`. */
    txt : txt,

    /** `dom (name, atts, child0, ...)` is an element `name` with attributes 
        taken from the `atts` object. `atts` is optional. `child0` can be 
        a regular string or an array of dom objects. */
    dom : dom,

    /** `set_css_width (e, w)` sets the width of `e` to `w`. */
    set_css_width : set_css_width,

    /** `set_height (e, h)` sets the height of `e` to `h`. */
    set_css_height : set_css_height,

    /** `set_dim (e, w, h)` set the width and height of `e` to `w` and `h`. */
    set_css_dim : set_css_dim,

    /** 
     * `classify (el, c, is_c)` changes the class `c` of the element `el`. 
     * If `is_c` is `true` the class is added, otherwise removed. 
     * Returns the previous `is_c` value. 
     */
    classify : classify 
  };
} ();
/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** UI messages */
var Msg =
{ 
  "en" :
  {
    "l_preview" : "Preview",
    "l_preview_tip" : "Show the thumbnails of the document's pages",
    "l_record" : "Record",
    "l_record_tip" : "Show the document's record",
    "l_pdf" : "PDF",
    "l_pdf_tip" : "Download the document in PDF format",
    "l_txt" : "TXT",
    "l_txt_tip" : "Download the document in text format",
    "l_divs" : "Division",
    "l_divs_tip" : "Show the document's table of logical divisions",
    "l_page_abbr" : "p.\u2009",
    "l_fit" : "Fit",
    "l_fit_tip" : "Scale to fit the page",
    "l_width" : "Width",
    "l_width_tip" : "Scale to fit the page width",
    "l_actual" : "1:1",
    "l_actual_tip" : "Scale to the page's physical size",
    "l_pct" : "1000%",
    "l_pct_tip" : "Scale to percentage",
    "l_unit_label" : "1cm",
    "l_search" : "Search",
    "l_terms" : "Terms",
    "l_terms_tip" : "Highlight search terms",
    "l_text" : "Text",
    "l_text_tip" : "Show the text of the page",
    "l_library" : "library",
    "l_library_tip" : "Browse and search the digital library",
    "l_options" : "Options",
    "l_langs" : "en",
    "l_help" : "Help",
    "l_help_tip" : "Show help"
  },

  "fr" : 
  {
    "l_preview" : "Aperçu",
    "l_preview_tip" : "Afficher les vignettes des pages du document",
    "l_record" : "Notice",
    "l_record_tip" : "Afficher la notice du document",
    "l_pdf" : "PDF",
    "l_pdf_tip" : "Télécharger le document au format PDF",
    "l_txt" : "TXT",
    "l_txt_tip" : "Télécharger le document au format texte",
    "l_divs" : "Divisions",
    "l_divs_tip" : "Afficher la table des divisions logiques du document",
    "l_page_abbr" : "p.\u2009",
    "l_fit" : "Adapter",
    "l_fit_tip" : "Afficher la page entière",
    "l_width" : "Largeur",
    "l_width_tip" : "Afficher la page dans sa largeur",
    "l_actual" : "1:1",
    "l_actual_tip" : "Afficher la page dans sa taille physique",
    "l_pct" : "1000%",
    "l_pct_tip" : "Afficher la page au niveau de pourcentage",
    "l_unit_label" : "1cm",
    "l_search" : "Rechercher",
    "l_terms" : "Termes",
    "l_terms_tip" : "Mettre en évidence les termes recherchés",
    "l_text" : "Texte",
    "l_text_tip" : "Montrer le texte de la page",
    "l_library" : "Bibliothèque",
    "l_library_tip" : "Parcourir et rechercher la bibliotèque numérique",
    "l_options" : "Options",
    "l_langs" : "fr",
    "l_help" : "Aide",
    "l_help_tip" : "Afficher l'aide"
  }
} 
/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** Server request module. */ 

var Request = function () 
{

  function http ()
  {
    if (window.XMLHttpRequest) return new XMLHttpRequest ();
    if (window.ActiveXObject) return new ActiveXObject("Microsoft.XMLHTTP");
    Debug.log ("No HttpRequest object found");
    return null;
  }
  
  function get (uri, k) 
  {
    var r = http (); if (!r) return null;
    function kontinue () 
    { if (r.readyState == 4 /* DONE */) k(r.status, r.responseText); }
    r.onreadystatechange = kontinue;
    r.open ("GET", uri, true);
    r.send ();
    return function () { r.abort (); }
  }

  return { 
    /** 
     * `get (uri, k)` makes a GET on `uri` and returns the result to `[k]`.
     * Returns a closure that if invoked aborts the request or `null` if
     * HTTP requests cannot be made. 
     */
    get : get 
  };
} ();
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
/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** Rectangles.

    A rectangle is defined by a point (ox,oy), its {e origin} and 
    a size (w,h). Operations on rectangles with negative sizes are
    undefined.

    The surface S([r]) spanned by [r] is \[[ox]; [ox] + [w]\] x
    \[[oy]; [oy] + [h]\]. The extremum points of this space are
    the box's {e corners}. There is a distinguished n-dimensional
    [empty] box such that S([empty]) is empty. */

var Rrect = function () 
{  
  function err_e () { throw "empty rectangle"; }
  var empty = { empty : true }
  function pt (x, y) { return { x : x, y : y }; }
  function v (ox, oy, w, h) { return { ox : ox, oy : oy, w : w, h : h }; }
  function o (r) { return (r.empty) ? err_e () : pt (r.ox, r.oy); }
  function ox (r) { return (r.empty) ? err_e () : r.ox; }
  function oy (r) { return (r.empty) ? err_e () : r.oy; }
  function size (r) { return (r.empty) ? err_e () : { w : r.w, h : r.h }; }
  function w (r) { return (r.empty) ? err_e () : r.w; }
  function h (r) { return (r.empty) ? err_e () : r.h; }
  var zero = v (0, 0, 0, 0);
  var unit = v (0, 0, 1, 1);
  function of_ltrb (l, t, r, b) { return v (l, t, r - l, t - b); }

  /* Functions */

  var min = o;
  var minx = ox;
  var miny = oy;
  function max (r) { return (r.empty) ? err_e () : pt (r.ox + r.w, r.oy + r.h);}
  function maxx (r) { return (r.empty) ? err_e () : r.ox + r.w; }
  function maxy (r) { return (r.empty) ? err_e () : r.oy + r.h; }
  function mid (r)
  { return (r.empty) ? err_e () : pt (r.ox + 0.5 * r.w, r.oy + 0.5 * r.h); }

  function midx (r) { return (r.empty) ? err_e () : r.ox + 0.5 * r.w; }
  function midy (r) { return (r.empty) ? err_e () : r.oy + 0.5 * r.h; }
  var bottom_left = min;
  var top_right = max;
  function bottom_right (r) 
  { return (r.empty) ? err_e () : pt (r.ox + r.w, r.oy); }

  function top_left (r) 
  { return (r.empty) ? err_e () : pt (r.ox, r.oy + r.h); }
  
  function area (r) { return (r.empty) ? 0 : r.w * r.h; }
  function inter (r1, r2) 
  {
    if (r1.empty || r2.empty) return empty;
    var l1 = r1.ox; var r1_ = l1 + r1.w;
    var l2 = r2.ox; var r2_ = l2 + r2.w;
    if ((r1_ < l2) || (r2_ < l1)) return empty; 
    var b1 = r1.oy; var t1 = b1 + r1.h;
    var b2 = r2.oy; var t2 = b2 + r2.h; 
    if ((t1 < b2) || (t2 < b1)) return empty;
    var ox = (l1 > l2) ? l1 : l2; 
    var oy = (b1 > b2) ? b1 : b2; 
    var w = ((r1_ < r2_) ? r1_ : r2_) - ox;
    var h = ((t1 < t2) ? t1 : t2) - oy; 
    return v (ox, oy, w, h);
  }

  function union (r1, r2)
  {
    if (r1.empty) return r2; 
    if (r2.empty) return r1; 
    var ox = (r1.ox < r2.ox) ? r1.ox : r2.ox; 
    var oy = (r1.oy < r2.oy) ? r1.oy : r2.oy;
    var r1_ = r1.ox + r1.w; var r2_ = r2.ox + r2.w; 
    var w = ((r1_ > r2_) ? r1_ : r2_) - ox;
    var t1 = r1.oy + r1.h; var t2 = r2.oy + r2.h; 
    var h = ((t1 > t2) ? t1 : t2) - oy;
    return v (ox, oy, w, h)
  }
    
  function inset (d, r) 
  {
    if (r.empty) return r;
    var ox = r.ox + d.x; 
    var oy = r.oy + d.y; 
    var w = r.w - 2 * d.x;
    var h = r.h - 2 * d.y; 
    return (w < 0 || h < 0) ? empty : v (ox, oy, w, h);
  }

  function round (r) 
  {
    if (r.empty) return r; 
    var ox = Math.floor (r.ox); 
    var oy = Math.floor (r.oy);
    var w = (r.w === 0 && ox !== r.ox) ? 1. : Math.ceil (r.w);
    var h = (r.h === 0 && oy !== r.oy) ? 1. : Math.ceil (r.h);
    v (ox, oy, w, h);
  }

  function map_f (f, r) 
  { return (r.empty) ? r : v (f (r.ox), f (r.oy), f (r.w), f (r.h)); }

  /* Predicates and comparison */

  function is_empty (r) { return r.empty; }
  function is_pt (r) { return (r.empty) ? false : r.w === 0 && r.h === 0; }
  function is_seg (r) 
  { 
    return (r.empty) ? false : 
    (r.w === 0 && r.h !== 0) || (r.w !== 0 && r.h === 0); 
  }

  function isects (r1, r2) 
  {
    if (r1.empty || r2.empty) return false; 
    var l1 = r1.ox; var r1_ = l1 + r1.w;
    var l2 = r2.ox; var r2_ = l2 + r2.w; 
    if ((r1_ < l2) || (r2_ < l1)) return false;
    var b1 = r1.oy; var t1 = b1 + r1.h; 
    var b2 = r2.oy; var t2 = b2 + r2.h; 
    if ((t1 < b2) || (t2 < b1)) return false; 
    return true;
  }

  function subset (r1, r2) 
  {
    if (r2.empty) return false;
    if (r1.empty) return true;
    return (r2.ox <= r1.ox) && (r2.oy <= r1.oy) && 
      (r1.w <= r2.w) && (r1.h <= r2.h);
  }

  function mem (pt, r) 
  {
    if (r.empty) return false; 
    return (r.ox <= pt.x) && (pt.x <= r.ox + r.w) && 
      (r.oy <= pt.y) && (pt.y <= r.oy + r.h);
  }
  
  function to_string (r) 
  {
    if (r.empty) return "(rect empty)";
    return "(rect (o " + r.x + " " + r.y + ") (size " + r.w + " " + r.h + "))";
  }

  return { 
    /** [empty] is the empty rectangle. */
    empty : empty,

    /** [v ox oy w h] is a rectangle with origin (ox,oy) and size (w,h). */
    v : v, 

    /** [o r] is the origin of [r]. */
    o : o,
    
    /** [ox r] is [(o r).x]. */
    ox : ox,

    /** [oy r] is [(o r).y]. */
    oy : oy,
    
    /** [size r] is the size of [r]. */
    size : size,

    /** [w r] is [(size r).w]. */
    w : w,

    /** [h r] is [(size r).h]. */
    h : h,

    /** [zero] is a rectangle with zero origin and size. */
    zero : zero,

    /** [unit] is a rectangle with zero origin and unit size. */
    unit : unit,

    /** [of_ltrb] is a rectangle from left, top, right and bottom coords. */
    of_ltrb : of_ltrb,
    
    /** Functions **/

    /** [min r] is the smallest point of [r] (its origin). */
    min : min,

    /** [minx r] is [(min r).x]. */
    minx : minx,

    /** [miny r] is [snd (min r)]. */
    miny : miny,

    /** [max r] is the greatest point of [r] (its size added to the origin). */
    max : max,

    /** [maxx r] is [(max r).x]. */
    maxx : maxx,

    /** [maxy r] is [(max r).y]. */
    maxy : maxy,

    /** [mid r] is the mid point between [min] and [max]. */
    mid : mid,

    /** [midx r] is [(mid r).x]. */
    midx : midx,

    /** [midy r] is [(mid r).y]. */
    midy : midy,

    /** [bottom_left r] is the bottom-left corner of [r]. */
    bottom_left : bottom_left,

    /** [bottom_right r] is the bottom-right corner of [r]. */
    bottom_right : bottom_right,

    /** [top_left r] is the top-left corner of [r]. */
    top_left : top_left,

    /** [top_right r] is the top-right corner of [r]. */
    top_right : top_right,

    /** [area r] is the surface area of [r]. */
    area : area,

    /** [inter r r'] is a box whose space is the intersection of S([r]) 
        and S([r']). */
    inter : inter,

    /** [union r r'] is the smallest box whose space contains 
        S([r]) and S([r']). */
    union : union,

    /** [inset d r] is [r] whose edges are inset in each dimension
        according to amounts in [d]. Negative values in [d] outset. If
        the resulting size is negative returns {!empty}.  Returns
        {!empty} on {!empty}. */
    inset : inset,

    /** [round r] is the smallest box containing [r] with integer valued
        corners. Returns {!empty} on {!empty}. */
    round : round,

    /** [map_f f r] is the box whose origin and size are those of [b] with
        their components mapped by [f]. Returns {!empty} on {!empty}. */
    map_f : map_f,
  

    /** Predicates and comparison */

    /** [is_empty r] is [true] iff [r] is {!empty}. */
    is_empty : is_empty,

    /** [is_pt r] is [true] iff [r] is not {!empty} and its size is equal
        to 0 in every dimension. */
    is_pt : is_pt,

    /** [is_seg r] is [true] iff [r] is not {!empty} and its size is 
        equal to 0 in exactly one dimension. */
    is_seg : is_seg,

    /** [isects r r'] is [not (is_empty (inter r r'))]. */
    isects : isects,

    /** [subset r r'] is [true] iff S([r]) is included in S([r']). */
    subset : subset,

    /** [mem p r] is [true] iff [p] is in S([r]). */
    mem : mem,


    /** Printers */

    /** [to_string r] is a textual representation of [r]. */
    to_string : to_string
  }  
} ();
/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** The Ui module handles interaction with the DOM */

var Ui = function () 
{  

  function log (m) { Debug.log (m); /* TODO */ }
  function app_uri (ui, uri) { return ui.base_uri + "/" + uri; }

  function view_title_txt (p_str, d, v)
  {
    function sep (acc, c) { return (acc === "") ? " " + c : acc + "–" + c; }; 
    var view = d.views[v];
    var view_str = 
      (view.label) ? p_str + view.label : (v + 1).toString (); 
    var name = (d.name) ? d.name : d.indexes[0].name; 
    var keys = d.indexes[0].keys;    
    return name + J.fold (sep, "", keys) + " " + view_str;
  }

  function render_webpage_title (ui)
  {
    var view_title = view_title_txt (ui.msg["l_page_abbr"], 
                                 ui.state.doc, ui.state.view)
    document.title = view_title + " / " + ui.library; 
  }

  function render_view_selector (ui)
  {
    var d = ui.state.doc;
    var current = ui.state.view;
    var count = d.views.length;
    var div = J.dom ("div", {id : ui.view_sel.id });  
    if (count < 10) 
    {
      var page_abbr = null;
      for (var v = 0; v < count; v++)
      {
        var view = d.views[v]; 
        if (view.label && !page_abbr)
        {
          page_abbr = J.dom ("abbr", {}, ui.msg["l_page_abbr"]); 
          div.appendChild (page_abbr);
        }
        
        var a = null;
        if (current != v) 
        {
          a = J.dom ("a", { "href" : app_uri (ui, Doc.view_uri (d, v)) }); 
          J.ev (a, "click", J.app (ev_set_view, ui, v));
        } else {
          a = J.dom ("a", { "class" : "l_highlight" });  
        }
        if (view.label) a.appendChild (J.txt (view.label)); else
        { 
          a.appendChild (J.txt ("v")); 
          a.appendChild (J.dom ("sub", {}, v.toString ()))
        }
        div.appendChild (a);
      }
    }
    else 
    {
      div.appendChild (J.txt ("TODO"));
    }
    
    ui.view_sel.parentNode.replaceChild (div, ui.view_sel);
    ui.view_sel = div;
  }

  function render_canvas (ui)
  {
    if (ui.image.complete) 
    {
      var ctx = ui.canvas.getContext("2d");
      ui.canvas.width = ui.view.clientWidth;
      ui.canvas.height = ui.view.clientHeight;
//      var scale = ui.image.width / ui.canvas.width;
      var sw = ui.canvas_scale * ui.canvas.width; 
      var sh = ui.canvas_scale * ui.canvas.height;
      Debug.log ("Rendering ! canvas:%dx%d image:%dx%d scaled:%dx%d", 
                 ui.canvas.width,
                 ui.canvas.height,
                 ui.image.width,
                 ui.image.height,
                 sw, sh);
      ctx.drawImage (ui.image, 
                     0, 0, sw, sh,
                     0, 0, ui.canvas.width, ui.canvas.height); 
    }
  }

  function scale_to (ui, scale)
  {
    switch (scale) 
    {
    case Doc.Scale.Fit: 
      var wscale = ui.view.clientWidth / ui.image.height; 
      var hscale = ui.view.clientHeight / ui.image.width; 
      Debug.log ("FIT: h:%f w:%f", wscale, hscale);
      ui.canvas_scale = Math.min (wscale, hscale);
      break;
    case Doc.Scale.Width: 
      ui.canvas_scale = ui.image.width / ui.view.clientWidth;
      break; 
    case Doc.Scale.Real:
      Debug.log ("REAL !");
      break;
    case Doc.Scale.Custom: 
      Debug.log ("CUSTOM !");
      break; 
    default: J.assert (false); break;
    }
    render_canvas (ui);
  }



  function render_view_img (ui)
  {
    var swap = ui.canvas;
    ui.canvas = ui.canvas_last; 
    ui.canvas_last = swap;
    ui.image = new Image ();

    function finish () 
    {
      if (ui.image.complete)
      {
        render_canvas (ui); 
        J.classify (ui.canvas, "la_enter_right", false);
        J.classify (ui.canvas_last, "la_enter_right", true);
      }
    }

    ui.image.onload = function () { finish (); }
    ui.image.onerror = function () { Debug.log ("Image could not be loaded"); }
    ui.image.src = Doc.view_image_uri (ui.state.doc, ui.state.view);;    
  }

  function render (ui)
  {
    render_view_img (ui);
    render_view_selector (ui);
    render_webpage_title (ui);
    Debug.log ("Rendered: %s", document.title);
  }

  function history_push (ui, replace)
  {
    if (window.history)
    {
      var uri = app_uri (ui, Doc.view_uri (ui.state.doc, ui.state.view));
      (replace) ? 
        window.history.replaceState (ui.state, uri, uri) : 
        window.history.pushState (ui.state, uri, uri);
      
      Workaround.opera_history (); 
    } else { 
      Debug.log ("No window.history support"); // TODO
    }
  }
  
  function history_pop (ui, ev)
  { 
    Debug.log ("History : %o", ev.state); 
    if (ev.state != null) { ui.state = ev.state; render (ui); } 
  }
  
  function cmd_set_view (ui, view) 
  { 
    ui.state.view = view;
    history_push (ui, false); // Before render for correct titles in hist.
    render (ui);
  }

  function cmd_prev_view (ui) 
  {
    if (ui.state.view === 0) return /* TODO bump */; 
    cmd_set_view (ui, ui.state.view - 1);
  }

  function cmd_next_view (ui) 
  {
    if (ui.state.view === ui.state.doc.views.length - 1) return /* TODO bump */;
    cmd_set_view (ui, ui.state.view + 1);
  }
      
  
  function ev_set_view (ui, view, ev)
  {
    ev.preventDefault();
    cmd_set_view (ui, view);
    return false;
  }
      
  function show_record (ui, ev) { Debug.log ("Show record %o !", ev); }
  function show_preview (ui, ev){ Debug.log ("Show preview %o !", ev); }
  function show_divisions (ui, ev){ Debug.log ("Show division %o !", ev); }
  
  function button (ui, id, onclick)
  {
    var b = J.$(id); 
    var label = document.createTextNode (ui.msg[id]); 
    b.appendChild(label);
    //      b.style.textTransform = "uppercase"; // work around webkit bug. 
      b.title = ui.msg[id + "_tip"]; 
    b.href = ui.loc.toString() + "#" + ui.msg[id]; // TODO refine
    if (onclick) J.ev (b, "click", J.app (onclick, ui));
    return b;
  }

  function keyboard (ui, ev)
  {
    var key = String.fromCharCode (ev.keyCode || ev.which);
    switch (key) 
    {
    case "P": case "%" /* left-arrow */: cmd_prev_view (ui); break;
    case "N": case "'" /* right-arrow */:  cmd_next_view (ui); break;
    case "&" /* up-arrow */: Debug.log("up-arrow"); break;
    case "(" /* down-arrow */: Debug.log("down-arrow"); break;
    default: Debug.log("Unhandled key:%o ev:%o", key, ev); break;
    }
  }

  
  function find_lang (langs, prefix)
  {
    var lang = null;
    for (var p in langs) if (p == prefix) { lang = langs[p]; break; }
    return lang; // TODO error handling
  }
  

  function init_uri (ui, uri)
  {
    ui.state.doc = Doc.of_uri (uri);
    ui.state.view = 
      (uri[uri.length - 2] !== "v") ? 
      0 /* TODO */ : parseInt(uri[uri.length - 1], 10) - 1;
    render (ui);
    history_push (ui, true);
  }

  function create (conf, loc) 
  {
    if (!conf) { conf = { base : "" }; }
    var ui =
      { 
        library : null,
        loc : null, lang : null, msg : null, show : null, base_uri : null,
        state : { doc : null, view : null },
        state_volatile : 
        {
          fullscreen : false, 
          bar_visible : false, 
          info_visible : false,
          divs_visible : false, 
          help_visible : false 
        },
        pages : null,
        unit : 
        {
          slider : null, span : null, label : null
        },
        opts : 
        {
          library : null, options : null, langs : null, help : null
        },
        view : null,
        canvas_last : null,
        canvas : null,
        canvas_scale : 1.0,
        image : null
      };
    
    ui.loc = loc;
    var root = ui.loc.pathname;
    if (root.indexOf(conf.base) != 0) 
    {
      log ("The application is not published under the base URI '" 
           + conf.base + "' (current location is '" + root + "')");
    }
    var req = root.slice(conf.base.length).split("/");
    ui.lang = find_lang (conf.langs, req[0]);
    ui.msg = Msg[ui.lang];
    ui.show = req.slice(1);
    ui.base_uri = conf.base + req[0]; 
    Debug.log ("Luigi for: '%s' lang: '%s' show: %o", root, ui.lang, ui.show);
    
    ui.nav = 
      { record : button (ui, "l_record", show_record),
        divisions : button (ui, "l_divs", show_divisions),
        preview : button (ui, "l_preview", show_preview), 
        pdf : button (ui, "l_pdf", null),
        txt : button (ui, "l_txt", null) }
    
    ui.view_sel = J.$("l_doc_view_sel");
    ui.scale = 
      { fit : button (ui, "l_fit", J.app (scale_to, ui, Doc.Scale.Fit)),
        width : button (ui, "l_width", J.app (scale_to, ui, Doc.Scale.Width)),
        actual : button (ui, "l_actual", J.app (scale_to, ui, Doc.Scale.Real)),
        pct : button (ui, "l_pct", J.app (scale_to, ui, Doc.Scale.Custom)) }
    
    ui.unit.slider = J.$("l_unit"); 
    ui.unit.span = J.$("l_unit_span"); 
    ui.unit.label = button (ui, "l_unit_label", null);
    
    ui.search = 
      { search : button (ui, "l_search", null),
        terms : button (ui, "l_terms", null),
        text : button (ui, "l_text", null) }
    
    ui.opts.library = button (ui, "l_library", null);
    ui.library = conf.library[ui.lang];
    if (ui.library) ui.opts.library.innerHTML = ui.library;
    
    ui.opts.opts = button (ui, "l_options", null); 
    ui.opts.opts.innerHTML += " ▾";
    ui.opts.langs = button (ui, "l_langs", null); 
    ui.opts.langs.innerHTML += " ▾";
    ui.opts.help = button (ui, "l_help", null);

    ui.view = J.$("l_view");
    ui.canvas_last = J.dom ("canvas", { "class" : "l_canvas la_enter_right" });
    ui.canvas = J.dom ("canvas", { "class" : "l_canvas" });
    ui.canvas_scale = 1.0; 
    ui.view.appendChild (ui.canvas_last); 
    ui.view.appendChild (ui.canvas); 

    J.ev (document, "keyup", J.app (keyboard, ui)); // TODO more fine grained. 
    J.ev (window, "popstate", J.app (history_pop, ui)); 
    J.ev (window, "resize", function (ev) { render_canvas (ui); });
    init_uri (ui, req.slice(1));
    return ui;
  }; 

  return { 
    /**  
     * `create (conf, loc)` creates and populates the ui 
     * with configuratino `conf` and location `loc`.
     */
    create : create 
  };
} (); 
/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** The Workaround module handles browser compatibility workarounds., */

var Workaround = function () 
{  
  function opera_history () 
  {
//    document.getElementsByTagName("base")[0].href =
//      document.getElementsByTagName("base")[0].href;
  }
  
  var transitionend = function () 
  {
    var props = 
      { 'transition':'transitionend',
        'OTransition':'oTransitionEnd',
        'MSTransition':'msTransitionEnd',
        'MozTransition':'transitionend',
        'WebkitTransition':'webkitTransitionEnd' }; 
    var e = document.createElement ("dummy");  
    for (var p in props) if (e.style[p] !== undefined) return props[p];
    return null;
  } (); 

  return { 
    /** 
     * Hack for HTML5 history relative link bug present in Opera < 11.60.
     * Invoke after pushState.
     * http://my.opera.com/community/forums/topic.dml?id=1185462
     */
    opera_history : opera_history,
    
    /** 
     * `transitionend` is the browser specific CSS3 `transitionend`
     *  event name.
     */
    transitionend : transitionend  }; 
} ();
/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/** Luigi main. */

var Luigi = function () 
{
  var init = false;
  var luigi_conf = 
    {
      base : "/luigi/",
      langs : { "en": "en", "fr" : "fr" },
      library : { "en" : "Digital CIRA", "fr" : "CIRA numérique" }
    };
  
  function main () 
  { 
    if (!init) 
    {
      init = true;
      var ui = Ui.create (luigi_conf, window.location); 
      if (DEBUG)
      {
        // Convenient global environment pollution.
        window.luigi_conf = luigi_conf;
        window.ui = ui; 
        window.J = J;
        window.Workaround = Workaround;
        window.Debug = Debug;
        window.Request = Request;
        window.Doc = Doc;
        window.Ui = Ui;
      }
    }
  }
  document.addEventListener("DOMContentLoaded", main, false);
} (); 



