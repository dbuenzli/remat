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
