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

