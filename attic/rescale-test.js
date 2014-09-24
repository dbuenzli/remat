/*--------------------------------------------------------------------------- 
   Copyright 2012 Daniel C. Bünzli. All rights reserved. 
   Distributed under a BSD3 license. 
  ---------------------------------------------------------------------------*/

/* Image and screen resolution. */

var csspx_per_cm = 38; /* 1cm = 38 css px  */
function css_px_dim (i) 
{ 
  return { w : Math.round ((i.width / i.ppcm) * csspx_per_cm), 
           h : Math.round ((i.height / i.ppcm) * csspx_per_cm) }
}

function device_pixel_ratio ()
{
  if (!window.devicePixelRatio) 
  { window.console.log 
    ("Warning. window.devicePixelRatio is undefined, assuming 1");
  }
  return (window.devicePixelRatio || 1); 
}

function pixel_grid (i)
{
  var ip = i.data; var x = 0; var y = 0; var j = 0; var off = 0; 
  for (y = 0; y < i.height; y++)
  {
    j = y * i.width; 
    for (x = 0; x < i.width; x++)
    {
      off = (j + x) << 2;
      if (y % 2 === 1) 
      { ip[off] = 255; ip [++off] = 0; ip[++off] = 0; ip [++off] = 255; }
/*      else if (x % 2 === 1) 
      { ip[off] = 0; ip [++off] = 255; ip[++off] = 0; ip [++off] = 255; } */
    }
  }
}


/* Remember ui state */

function save_prefs (selected, show_grid)
{
  window.localStorage.img_selected = window.JSON.stringify (selected); 
  window.localStorage.img_show_grid = window.JSON.stringify (show_grid);
}

function get_prefs ()
{
  var selected = window.localStorage.img_selected;
  var show_grid = window.localStorage.img_show_grid;
  return { selected : (selected) ? window.JSON.parse (selected) : 0, 
           show_grid : (show_grid) ? window.JSON.parse (show_grid) : false };
}

/* Element classification */

function tag_ui (e) { J.classify (e, "ui", true); return e; }
function tag_col (e) { J.classify (e, "col", true); return e; }
function tag_col_title (e) { J.classify (e, "col-title", true); return e; }
function tag_button (e) { J.classify (e, "button", true); return e; }
function tag_scale (e) { J.classify (e, "scale", true); return e; }
function tag_screen_info (e) { J.classify (e, "screen-info", true); return e; }
function tag_technique (e) { J.classify (e, "technique", true); return e; }
function tag_resample (e) { J.classify (e, "resample", true); return e; }
function tag_scale_label (e) { J.classify (e, "scale-label", true); return e; }
function tag_swap_button (e) { J.classify (e, "swap-button", true); return e; }
function tag_rescales (e) { J.classify (e, "rescales", true); return e; }

/* UI elements */

function ui_screen_info ()
{
  var r = window.devicePixelRatio || 1;
  var txt = "Screen @ " + (r * csspx_per_cm) + " ppcm";
  txt += (window.devicePixelRatio) ? "" : " (CSS pixel fallback)";
  return tag_screen_info (J.dom ("div", txt));
}

function ui_image_spec (imgs, prefs, on_spec_change)
{
  function img_opt (j, img)
  {
    var txt = img.title + " @ " + img.ppcm + " ppcm"; 
    return J.dom ("option", { value : j }, txt); 
  }
  var opts = J.mapi (img_opt, imgs);
  var sel = J.dom ("select", { selectedIndex : prefs.selected }, opts);
  var grid = J.dom ("input", { type : "checkbox", checked : prefs.show_grid }); 
  var form = J.dom ("form", 
                    J.dom ("label", "Image: ", sel),
                    J.dom ("label", grid, " Show pixel grid")); 
                    
  var last = null; 
  function on_change (e) 
  { 
    var now = { abort : false }; 
    function abort () { return now.abort; }
    if (last) last.abort = true; 
    last = now; 
    save_prefs (sel.selectedIndex, grid.checked);
    on_spec_change (abort, imgs [sel.selectedIndex], grid.checked);
  }
  J.ev (form, "change", on_change);
  on_change (null);
  return form;
}
                       
function ui_scale_col (scales, base_height) 
{
  function label (s)
  {
    var pct = Math.floor (s * 100) + "%";
    var label = J.set_css_height (J.dom ("div", pct), 
                                  Math.round (base_height * s));
    return tag_scale_label (label);
  }
  var title = tag_col_title (J.dom ("div")); 
  var col = tag_col (J.dom ("div", title, J.map (label, scales))); 
  return tag_scale (col);
}

function ui_swap_button ()
{  
  function move_right (e)
  {
    if (e.nextElementSibling === null) return; 
    e.parentElement.insertBefore(e.nextElementSibling, e);
  }
  
  var swap = tag_swap_button (J.dom ("div", "< >"));
  J.ev (swap, "mousedown", function (e) 
        { move_right (swap.parentElement); });
  return swap;
}

function ui_rescales (scales, techniques, abort, i, show_grid)
{
  var se = J.dom ("canvas"); 
  se.width = i.width; 
  se.height = i.height; 
  se.ppcm = i.ppcm;
  var sx = se.getContext ("2d");
  sx.drawImage (i, 0, 0); 
  var si = sx.getImageData (0, 0, i.width, i.height); 
  si.ppcm = i.ppcm; 

  var dim = css_px_dim (i);
  function tech_col (t) 
  {
    var col_width = dim.w * (J.fold (Math.max, 0, scales)) + csspx_per_cm;
    function row (scale)
    {
      var i = t.scale (abort, se, si, show_grid, scale);
      J.ev (i, "click", function (e){ window.open (i.toDataURL("image/png"));});
      var row = J.set_css_height (J.dom ("div", i), Math.round (dim.h * scale));
      return tag_resample (row); 
    }
    
    var title = tag_col_title (J.dom ("div", t.title)); 
    var rows = J.map (row, scales);
    var col = tag_col (J.dom ("div", title, ui_swap_button (), rows)); 
    return tag_technique (J.set_css_width (col, col_width));
  }

  var scale_col = ui_scale_col (scales, dim.h);  
  return tag_rescales (J.dom ("div", scale_col, J.map (tech_col, techniques)));
}

/* Resample techniques */

function t_rescale (filter, abort, se, si, show_grid, scale)
{
  if (abort ()) return; 
  var dim = css_px_dim (si); 
  var w = Math.round (dim.w * scale); var h = Math.round (dim.h * scale); 
  var de = J.dom ("canvas"); 
  J.set_css_dim (de, w, h); 
  de.width = Math.round (w * device_pixel_ratio ()); 
  de.height = Math.round (h * device_pixel_ratio ()); 
  var dx = de.getContext ("2d"); 
  var di = dx.createImageData (de.width, de.height); 
  Rescale.image (filter, si, di, abort, function (abort, di)
  {
    if (abort ()) return; 
    if (show_grid) pixel_grid (di);
    
    Rescale.linear_to_sRGB (di, abort, function (abort, di)
    {
      if (abort ()) return; 
      dx.putImageData (di, 0, 0);
    }); 
  });
  return de;
}

function t_canvas_matrix (abort, se, si, show_grid, scale)
{
  if (abort ()) return;
  var dim = css_px_dim (si); 
  var w = Math.round (dim.w * scale); var h = Math.round (dim.h * scale); 
  var de = J.dom ("canvas"); 
  J.set_css_dim (de, w, h); 
  de.width = si.width; 
  de.height = si.height; 

  var dx = de.getContext ("2d"); 
  dx.scale (scale, scale); 
  dx.putImageData (si, 0, 0); 
  return de;
}

function t_canvas_css (abort, se, si, show_grid, scale)
{
  if (abort ()) return;
  var dim = css_px_dim (si); 
  var w = Math.round (dim.w * scale); var h = Math.round (dim.h * scale); 
  var de = J.dom ("canvas"); 
  J.set_css_dim (de, w, h);
  de.width = se.width; 
  de.height = se.height; 
  var dx = de.getContext ("2d"); 
  dx.drawImage (se, 0, 0); 
  return de;
}

function inject_rescales (into, scales, techniques, abort, i, show_grid)
{
  var img = new Image; img.src = i.uri; img.ppcm = i.ppcm;
  img.onload = function () 
  {
    var id = "result";
    if (abort ()) return;
    var old = J.$(id); if (old) old.parentElement.removeChild (old);
    var fresh = ui_rescales (scales, techniques, abort, img, show_grid);
    fresh.id = id;
    into.appendChild (fresh); 
  }
}

function main () 
{
  var prefs = get_prefs (); 
  var scales = [0.25, 0.375, 0.5, 0.75, 1.0, 1.5, 2];
  var techniques = [ 
    { title : "Binary pixel mixing", 
      scale : J.app (t_rescale, Rescale.Filter.Binary_pixel_mixing) },
    { title : "Binary subpixel mixing (3-tap filter)", 
      scale : J.app (t_rescale, Rescale.Filter.Binary_subpixel_mixing3) },
    { title : "Binary supixel mixing (5-tap filter)", 
      scale : J.app (t_rescale, Rescale.Filter.Binary_subpixel_mixing5) },
    { title : "Linear",
      scale : J.app (t_rescale, Rescale.Filter.Linear) },
    { title : "Box (nearest neighbor)",
      scale : J.app (t_rescale, Rescale.Filter.Box) },
    { title : "B-spline (cubic)",
      scale : J.app (t_rescale, Rescale.Filter.B_spline) },
    { title : "Catmull-Rom (cubic)",
      scale : J.app (t_rescale, Rescale.Filter.Catmull_rom) },
    { title : "Lanczos 3", 
      scale : J.app (t_rescale, Rescale.Filter.Lanczos_3) },
    { title : "Canvas matrix (browser dependent)",
      scale : t_canvas_matrix }, 
    { title : "Canvas CSS resize (browser dependent)",
      scale : t_canvas_css }]; 
      
  var imgs = 
    [{ title : "Binary image", 
       uri : "images/capital-cut.png", 
       ppcm : 118 },
     { title : "Binary text square", 
       uri : "images/text-cut.png",
       ppcm : 118 },
     { title : "Binary text rect", 
       uri : "images/text-cut2.png",
       ppcm : 118 },
     { title : "Color Square 2x2",
       uri : "images/colorsquare2.png", 
       ppcm : 1 },
     { title : "Color Square 4x4",
       uri : "images/colorsquare4.png",
       ppcm : 2 },
     { title : "RGB stripes 3x4",
       uri : "images/rgb34.png",
       ppcm : 1 },
     { title : "RGB stripes 3x4",
       uri : "images/rgb34@118.png",
       ppcm : 118 },
     { title : "RG stripes 2x4",
       uri : "images/rg24.png",
       ppcm : 1 },
     { title : "Rings 1000x1000",
       uri : "images/rings.gif",
       ppcm : 96 },
     { title : "Smile",
       uri : "images/smile1.png",
       ppcm : 20 },
     { title : "Gamma",
       uri : "images/rasp-grayator.png",
       ppcm : 38 }
    ];

  if (prefs.selected > imgs.length - 1) prefs.selected = 0;

  var on_change = J.app (inject_rescales, document.body, scales, techniques); 
  var ui_img_spec = ui_image_spec (imgs, prefs, on_change); 
  var ui = tag_ui (J.dom ("div", ui_screen_info (), ui_img_spec));
  document.body.appendChild (ui);
}

window.onload = main;
window.J = J

