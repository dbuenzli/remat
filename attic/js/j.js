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
