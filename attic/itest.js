
function css_dim (i) 
{
  return { w : Math.ceil (i.width * (96 / 300)), 
           h : Math.ceil (i.height * (96 / 300)) }
}

function draw_var_canvas (i, scale)
{
  var dim = css_dim (i);
  var ce = J.dom ("canvas"); 
  ce.style.width =  dim.w + "px";  /* 300 dpi */
  ce.style.height =  dim.h + "px"; 
  ce.width = scale * i.width; 
  ce.height = scale * i.height;
  var c = ce.getContext("2d");
  c.drawImage (i, 0, 0, i.width, i.height, 
                  0, 0, i.width * scale * scale, i.height * scale * scale);
  return ce;
}

function draw_fix_canvas (pixel_ratio, i, scale) 
{
  var dim = css_dim (i);
  var ce = J.dom ("canvas"); 
  ce.style.width = dim.w + "px"; 
  ce.style.height = dim.h + "px";
  ce.width = dim.w * pixel_ratio; 
  ce.height = dim.h * pixel_ratio;
  var c = ce.getContext ("2d"); 
  if (scale >= 1)
    c.drawImage (i, 0, 0, i.width / scale, i.height / scale, 
                 0, 0, ce.width, ce.width); 
  if (scale < 1) 
    c.drawImage (i, 0, 0, i.width, i.height, 
                 0, 0, ce.width * scale, ce.width * scale); 

  return ce;
}

function draw_fix_canvas_bis (i, scale) 
{
  var dim = css_dim (i);
  var ce = J.dom ("canvas"); 
  ce.style.width = dim.w + "px"; 
  ce.style.height = dim.h + "px";
  ce.width = i.width; 
  ce.height = i.height;
  var c = ce.getContext ("2d"); 
  if (scale >= 1)
    c.drawImage (i, 0, 0, i.width / scale, i.height / scale, 
                 0, 0, ce.width, ce.width); 
  if (scale < 1) 
    c.drawImage (i, 0, 0, i.width, i.height, 
                 0, 0, ce.width * scale, ce.width * scale); 

  return ce;
}

function draw_ctx_scale (i, scale) 
{
  var dim = css_dim (i); 
  var ce = J.dom ("canvas"); 
  ce.style.width = dim.w + "px"; 
  ce.style.height = dim.h + " px"; 
  ce.width = i.width; 
  ce.height = i.height; 
  var c = ce.getContext ("2d"); 
  c.scale (scale, scale); 
  c.drawImage(i, 0, 0);
  return ce;
}

function stripe (i)
{
  var ip = i.data; var x = 0; var y = 0; var j = 0; var off = 0; 
  for (y = 0; y < i.height; y++)
  {
    if (y % 2 === 1) 
    {
      j = y * i.width; 
      for (x = 0; x < i.width; x++)
      {
        off = (j + x) << 2
        ip[off] = 255; 
        ip[++off] = 0; 
        ip[++off] = 0; 
        ip[++off] = 255; 
      }
    }
  }
}


function draw_custom_resample (i, scale)
{
  var sce = J.dom ("canvas"); 
  sce.width = i.width; 
  sce.height = i.height;
  var sc = sce.getContext ("2d"); 
  sc.drawImage (i, 0, 0, i.width, i.height, 0, 0, i.width, i.height); 
  var src = sc.getImageData(0, 0, i.width, i.height); 
  
  var dce = J.dom ("canvas");
  dce.width = i.width * scale; 
  dce.height = i.width * scale;
  var dc = dce.getContext ("2d");
  var dst = dc.createImageData (dce.width, dce.height);
  var dim = css_dim (i);
  var ce = J.dom ("canvas"); 
  ce.style.width = dim.w + "px"; 
  ce.style.height = dim.h + "px";
  ce.width = dce.width; 
  ce.height = dce.height;
  var c = ce.getContext ("2d"); 

  Rescale.nearest (src, dst, function (cancel, dst) 
  {
    stripe (dst);               
    dc.putImageData (dst, 0, 0);   
    if (scale >= 1)
      c.drawImage (dce, 0, 0, dce.width / scale, dce.height / scale, 
                   0, 0, ce.width, ce.width); 
    if (scale < 1) 
      c.drawImage (dce, 0, 0, dce.width, dce.height, 
                   0, 0, ce.width * scale, ce.width * scale); 
    
  });
  return ce;
}

function draw_custom_resample_bis (i, scale)
{
  if (!window.devicePixelRatio) window.devicePixelRatio = 1;
  var sce = J.dom ("canvas"); 
  sce.width = i.width; 
  sce.height = i.height;
  var sc = sce.getContext ("2d"); 
  sc.drawImage (i, 0, 0, i.width, i.height, 0, 0, i.width, i.height); 
  var src = sc.getImageData(0, 0, i.width, i.height); 
  
  var dce = J.dom ("canvas");
  dce.width = i.width * scale * (96 * window.devicePixelRatio/ 300); 
  dce.height = i.width * scale * (96 * window.devicePixelRatio / 300);
  dce.style.width = (i.width * scale * (96 / 300)) + "px"; 
  dce.style.height = (i.width * scale * (96 / 300)) + "px"; 
  var dc = dce.getContext ("2d");
  var dst = dc.createImageData (dce.width, dce.height);
  var t = Math.random () + " "; 
  window.console.time (t);
  Rescale.nearest (src, dst, function (cancel, dst) 
  {
//    stripe(dst);
    dc.putImageData (dst, 0, 0);
    window.console.timeEnd (t);
  }); 

  var dim = css_dim (i);
  var d = J.dom ("div"); 
  d.style.width = dim.w + "px"; 
  d.style.height = dim.h + "px"; 
  d.style.overflow = "hidden"; 
  
  d.width = dce.width; // for draw_scale
  d.height = dce.height;
  d.appendChild (dce);

  return d;
}


function draw_technique (t, f, is)
{  
  var d = J.dom ("div", {}, J.dom ("h1", {}, J.txt (t))); 
  document.body.appendChild (d); 

  function draw_scale (i, scale) 
  {
    var ce = f (i, scale)
    var text = (Math.floor (scale * 100)) + "% "; 
    text += "canvas: " + ce.width + "x" + ce.height; 
    var b = J.dom ("div", {}, ce, J.txt (text));
    d.appendChild (b); 
  }

  function draw_scales (i) 
  {
    var scales = [0.5, 0.75, 1.0, 1.5, 2]; 
    J.iter (J.app (draw_scale, i), scales);
  }

  J.iter (draw_scales, is);
}




function main () 
{
  window.console.log ("ppcm: " + 
                      Math.ceil ((96 * window.devicePixelRatio) / 2.54));
  var loaded = 0;
  var srcs = ["images/capital-cut.png", "images/text-cut.png"]; 
  var is = []; 
  function draw () 
  {
    loaded++; 
    if (loaded < is.length) return;
    draw_technique ("Variable canvas size", draw_var_canvas, is);
    draw_technique ("Fixed canvas size (image size)", draw_fix_canvas_bis, is);
    draw_technique ("Fixed canvas size (1px per CSS px)", 
                    J.app (draw_fix_canvas, 1), is);     
    draw_technique ("Fixed canvas size (2px per CSS px)", 
                    J.app (draw_fix_canvas, 2), is);     
    draw_technique ("Fixed canvas size (3px per CSS px)", 
                    J.app (draw_fix_canvas, 3), is);     
    draw_technique ("Fixed canvas size (4px per CSS px)", 
                    J.app (draw_fix_canvas, 4), is);
    draw_technique ("Context scaling", draw_ctx_scale, is);    

    draw_technique ("Custom resample", draw_custom_resample, is);     
    draw_technique ("Custom resample (bis)", draw_custom_resample_bis, is);     
  }
  
  function image_of_src (src) 
  { var i = new Image; i.src = src; i.onload = draw; return i; }
  
  is = J.map (image_of_src, srcs); 
}

window.onload = main;
window.J = J

