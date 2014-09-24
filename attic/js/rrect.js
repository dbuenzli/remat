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
