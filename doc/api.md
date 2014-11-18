[HOWTO](howto.html) — [DESCRIBE](describe.html) — REST API

Remat REST API v0
=================

This document describes the Remat REST API that mediates the
interaction between a client and a server. It assumes basic knowledge
of Remat's data model.

The API allows to implement new clients (or servers) that can be used
with Remat's default server (or client). The primary goal of the API
is to *query* and *present* the document repository
contents. Modifying the repository via the API is currently not
supported. Except for media representations, all URI representations
are [JSON text](http://tools.ietf.org/html/rfc4627).

TODO
[json-schema](http://json-schema.org/latest/json-schema-core.html)
descriptions ? 


Conventions and basic types {#conventions}
------------------------------------------

The conventions and basic types of
[repository descriptions](describe.html#conventions) do
apply. Besides the following basic types are used in the API.

### Uri dereferencing

Relative URIs must be dereferenced *relative* to the URI used to get
the resource that mentions them. An example is provided in the next
section.
    
In the following example the `href` value is a `Locales.<Uri>`
value.

```json
{ "href": { "en": "books", "fr": "livres" } }
```

If this object was returned by `GET`ting the URI:

```
http://www.example.org/repo 
```

the specified URI is: 

* `http://www.example.org/books` for the english locale. 
* `http://www.example.org/livres` for the french locale.


### `Chunked` {#type-Chunked}

A `Chunked.<type>` is either an `Array.<type>` or an object with the
following members to get the array values chunked in URIs whose
representations are also `Chunked.<type>` *objects*.


| Member name    | Type                | 
|:---------------|:--------------------|:------------
| `first_href`   | `Uri`               | URI to first chunk 
| ?`prev_href`   | `Uri`               | URI to previous chunk
| ?`next_href`   | `Uri`               | URI to next chunk
| `last_href`    | `Uri`               | URI to last chunk
| `total_count`  | `Integer`           | Total number of values.
| `values`       | `Array.<type>`      | The chunk of values

The first chunk has no `prev_href` field. The last chunk has no
`next_href` field. Hence if both are absent all the values are in a single
chunk.

Base URI and Repository resource {#resource-repository}
-------------------------------------------------

The base URI, denoted by `$B`, is the only URI that needs to be
initially known to a client. It allows to get a hand on a repository
resource from which the service can be accessed by following
hyperlinks mentioned in the returned representation.

The base URI is also the root to express
[end user URIs](#end-user-URI) that allow users to bookmark 
specific parts of the repository's presentation.

The *repository* resource has the indexes of the repository and various
information about how it should be accessed and presented.

|                    | URI                       | Method | Representation
|:-------------------|:--------------------------|:-------|:--------------------
| Repository Resource   | `$B/api/repository.json`  | GET    | JSON `Repo`


### Type `Repo` {#type-Repo}

The `Repo` type is an object with the following members.

| Member name    | Type                 | 
|:---------------|:---------------------|:------------
| `version`      | `Integer`            | Remat API version number, currently 0.
| `locales`      | `Array.<`[`Locale_d`](describe.html#type-Locale_d)`>` \
| End-user locales.
| `name`         | `Locales.<String>`   | The repository name.
| `indexes`      | `Array.<Index>`      | Repository indexes.
| ?`search_href` | `Uri`                | URI for search queries.
| `publisher`    | `Locales.<String>`   | Name of the repository publisher.
| `publisher_href` | `Locales.<Uri>`    | Link to publisher webpage.


### Type `Index` {#type-Index}

An index is an arbitrary tree structure whose nodes are localized
headings and leaves are document synopses. The root index node define
the name of the index.

The `Index` type is an object with the following members.

| Member name    | Type                | 
|:---------------|:--------------------|:------------
| `ui_href`      | `String`            | User URI.
| `label`        | `Locales.<String>`   | Heading label.
| ?`headings_toc`| `Array.<Integer>`   \
| Indices of (sub)headings that should be in the (sub)headings toc. If absent\
all should be. If empty no toc.
| `headings`      | `Array.<Index>`     | (sub)headings.
| `synopses`      | `Locales.<Chunked.` | Document synopses (index leaves).
|                 | `<Synopsis>>`       |
| `synopses_isbd` | `Boolean`           \
| `true` if synopses descriptions are in ISBD format (used to detect titles).

The `headings*` and `synopses*` members are mutually exclusive.

**Note.** If the index *sort order* itself needs localisation, it is
performed at the level of the repository or document collection in the 
`indexes` field. 

### Type `Synopsis` {#type-Synopsis}

Document synopses are just a short description of a document (base
document or collection). 

The `Synopsis` type is an object the following members. 

| Member name    | Type                | 
|:---------------|:--------------------|:-------------------------------
| `doc_href`     | `Uri`               | URI to document description 
| `ui_href`      | `Uri`               | User URI
| `type`         | `Doc_type`          | Document type
| `descr`        | `Locales.<String>`  | Document description (e.g. ISBD).
| ?`doc_count`   | `Integer`           | Number of documents in a a collection

The `doc_count` member is present if and only if the synopsis `type`
is a collection.
  

## `Document` resource 

The *document* resource describes a document, that is either a
collection or a base document.

|                    | URI                     | Method | Representation
|:-------------------|:------------------------|:-------|:--------------
| Document Resource  | `Synopsis.doc_href`     | GET    | JSON `Document`

### Type `Document` {#type-Document}

The `Document` type is either an `Doc_collection` object or 
a `Doc_printed` object.

### Type `Doc_collection` {#type-Doc_collection}

The `Doc_collection` type is an object with the following members.

| Member name    | Type                 | 
|:---------------|:---------------------|:-------------------------------
| `type`         | `Doc_type`           | Document type, always 0.
| `title`        | `Locales.<String>`   | Collection title
| `summary`      | `Locales.<String>`   | Collection summary
| `indexes`      | `Array.<Index>`      | Collection indexes


### Type `Doc_printed` {#type-Doc_printed}

| Member name    | Type                 | 
|:---------------|:---------------------|:-------------------------------
| `type`         | `Doc_type`           | Document type, always 1.
| `title`        | `Locales.<String>`   | Document title
| ?`collection`  | `Doc_collection_ref` | Document collection reference. 
| ?`min_filter`  | `Doc_filter`  | Documentation minification filter.
| ?`mag_filter`  | `Doc_filter`  | Documentation magnification filter.
| ?`bilevel`     | `Boolean`     | True if all images are black and white.
| ?`scale`       | `Doc_scale`     | Initial scale
| ?`scale_value` | `Float`        | If `scale` is custom. 
| ?`views`       | `Chunked.<Doc_printed_view>` | Document views.

If the document belongs to a collection it has the `collection` member
with reference information. `{min,mag}_filter` indicate which filters
should be used when the view is mignified and magnified. If
unspecified the TODO `Repo.{min,mag}_filter[_bilevel]` is used.

### Type `Doc_printed_view` {#type-Doc_printed_view}

The `Doc_printed_view` corresponds to one document view (image).

| Member name    | Type                 | 
|:---------------|:---------------------|:-------------------------------
| ?`label`       | `Locales.<String>`   | Page label (e.g. number). 
| `thumb_href`   | `Uri`                | View image thumbnail uri.
| `image_href`   | `Uri`                | View image uri.
| ?`width`       | `Float`              | View image width in cm.
| ?`height`      | `Float`              | View image height in cm.

Both `width` and `height` are either present or absent.


End user URIs {#end-user-URI}
-----------------------------

Clients that render resources to end users should allow them to
bookmark them with good URIs expressed relative to the base URI
`$B`. With Remat's default server, these URI do not correspond to
anything on the server, they should silently redirect to the html
document defining the interface, the client will then analyse the URI
and make the appropriate calls on the API to render the contents of
the URI.

This section defines the mapping between end-user visible URIs and
corresponding actions that should be taken.  Remat's default web
client implements this mapping. On the server everything under `$B`
but `$B/api/` must silently redirect to `$B/api/ui/main.html`.

Below `a` is the repository object returned by `$B/api/repository.json`.

### Locale selection {#locale-selection}

The end-user URI defines the locale used in the interface as follows:

| URI pattern | Locale selection algorithm
|:------------|:-------------------------------------------
| `$B/$L/*`   \
| The locale is `l.locale` where `l` is the first object in the \
`a.locales` array such that `l.href = $L`.
| `$B/`        \
| Lookup a locale in the `a.locales` array with the browser language \
according to the [lookup algorithm][rfc4647] specified in RFC 4647. If none \
match fallback to the locale `a.locales[0].locale`.

[rfc4647]: http://tools.ietf.org/html/rfc4647#section-3.4

### URI user inteface outcomes

| URI pattern                   | Expected outcome 
|:--------------------------------|:-----------------------------------------
| `$B`                         \
| Redirect to appropriate locale `$L` according to browser language
| `$B/$L`                      | Redirect to `i/a.indexes[0]`
| `$B/$L/i/$I`                 | Contents of index `$I`
| `$B/$L/i/$I#k1-k2`           \
| Contents of index `$I` anchored at `k1-k2`
| `$B/$L/d/$D`                 \
| If `$D` printed document, redirect to `v/1`
| `$B/$L/d/$C`                 \
| If `$C` collection `c`, redirect to `i/c.indexes[0]`
| `$B/$L/d/$C/i/$I`              \
| If `$C` collection, contents of index `$I`
| `$B/$L/d/$C/i/$I#k1-k2`        \
| If `$C` collection, contents of index `$I` anchored at `k1-k2`
| `$B/$L/d/$C/d/k1/k2/` \
| If `$C` collection `c`, redirect to `../../../i/c.indexes[0]#k1-k2`




