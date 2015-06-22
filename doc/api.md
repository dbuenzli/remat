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

In the following example the `href` value is a `Locv.<Uri>`
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


### `Chunked` {#chunked}

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


### Type `Doc_ref` {#doc_ref}

A document reference is a short description and a link to a
document. Different document references may be used for the same
document in different contexts.

| Member name    | Type                |
|:---------------|:--------------------|:-------------------------------
| `data_href`     | `Uri`               | URI to document data
| `ui_href`      | `Uri`               | End-user URI to render document
| `descr`         | `String`           | Document description
| ?`descr_extra`  | `String`           | Additional description
| ?`date`        | [`Date_fuzzy`](describe.html#dates) | Document date
| ?`doc_type`     | [`Doc_type`](describe.html#doc_type) | Document type
| ?`doc_count`   | `Integer`           | Number of documents in a document set

The `doc_count` member may be present only if `doc_type` is a collection.


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
| Repository Resource   | `$B/api/repo.json`  | GET    | JSON [`Repo`](#repo)


## Type `Repo` {#repo}

The `Repo` type is an object with the following members.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `version`      | `Integer` | Remat API version number, currently 0.
| `name`         | `Locv.<String>`   | The repository name.
| `publisher`    | `Locv.<`[`Ui_link`](describe.html#ui_link)`>`  \
| Link to publisher.
| `ui_locales`   | `Array.<`[`Ui_locale`](describe.html#ui_locale)`>` \
| End-user locales.
| `indexes`      | `Array.<Locv<`[`Index`](#index)`>>` \
| References to repository indexes.
| ?`search_href` | `Uri`                | URI for search queries.


Indexes
-------

### Type `Index` {#index}

An `Index` object has the description of an index and a link to
the index data.

The index data is a tree whose internal nodes are headings and leaves
document references.


| Member name    | Type                |
|:---------------|:--------------------|:------------
| `name`         | [`Ui_link`](describe.html#ui_link) | Index name and UI href
| `node_href`   | `Uri`               \
| URI to toplevel [index node](#index_node)
| ?`descr`       | `String`            | Index description

The `heading` member of the root [node](#index_node) pointed by `node_href`
is equal to `name`.

### Type `Index_node` {#index_node}

The `Index_node` type is an object with the following members.

| Member name    | Type                |
|:---------------|:--------------------|:------------
| `heading`        | [`Ui_link`](describe.html#ui_link) \
| Heading label and UI href
| `in_toc`        | Boolean             | `true` if in table of contents
| `child_type`    | `"headings"` or `"doc_refs"` | children type
| `childs`    | `Array.<`[`Index_node`](#index_node)`>` or \
              `Array.<`[`Doc_ref`](#doc_ref)`>` | node children

Documents
---------

### Type `Doc` {#doc}

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `doc_type`     | [`Doc_type`](describe.html#doc_type) | Document type.
| `doc_href`     | `Locv.<`[`Uri`](#describe.html#uri)`>`   \
| Base end-user URI under the document is available.
| ?`doc_langs`    | `Array.<`[`Lang`](#lang)`>` \
| Languages present in the document.
| `doc_data` | [`Doc_print`](#doc_print) or [`Doc_set`](#doc_set) \
| Data specific to the `doc_type`

### Type `Doc_views` {#doc_views}

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `type` | [`Doc_views_type`](describe.html#doc_views_type) \
| The kind of document that is being viewed.
| ?`views`       | `Array.<`[`Doc_view`](#doc_view)`>` | Document views.
| TODO               |               |
| ?`min_filter`  | `Doc_filter`  | Documentation minification filter.
| ?`mag_filter`  | `Doc_filter`  | Documentation magnification filter.
| ?`scale`       | `Doc_scale`     | Initial scale
| ?`scale_value` | `Float`        | If `scale` is custom.


### Type `Doc_view` {#doc_view}

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| ?`label`       | `String` | View label
| `thumb_href`   | [`Uri`](describe.html#uri) | View thumbnail
| `image_href`   | [`Uri`](describe.html#uri) | View image
| TODO               |               |
| ?`width`       | `Float`              | View image width in cm.
| ?`height`      | `Float`              | View image height in cm.

### Type `Doc_set` {#doc_set}

A `Doc_set` is an heterogenous set of *atomic* documents. Documents
can be sorted according to publisher defined indexes.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `type` | [`Doc_set_type`](describe.html#doc_set_type) \
| Nature of the document set
| `indexes`      | `Array.<Locv<`[`Index`](#index)`>>` \
| References to document indexes.


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
`a.locales` array such that `l.ui_href = $L`.
| `$B/`        \
| Lookup a locale in the `a.locales` array with the browser language \
according to the [lookup algorithm][rfc4647] specified in RFC 4647. If none \
match fallback to the locale `a.locales[0].locale`.

[rfc4647]: http://tools.ietf.org/html/rfc4647#section-3.4

### URI user inteface outcomes

| URI pattern                   | Expected outcome
|:--------------------------------|:-----------------------------------------
| `$B`                         \
| Redirect to appropriate locale `$L` according to browser language (see above)
| `$B/$L`                      | Redirect to index list
| `$B/$L/i/$I`                 | Index list
| `$B/$L/i/$I`                 | Contents of index `$I`
| `$B/$L/i/$I#k1-k2`           \
| Contents of index `$I` anchored at `k1-k2`
| `$B/$L/d/`                 \
| Redirect to `$B/$L`
| `$B/$L/d/$D`                 \
| If `$D` printed document, redirect to `v/1`
| `$B/$L/d/$C`                 \
| If `$C` collection `c`, redirect to `i/c.indexes[0]`
| `$B/$L/d/$C/i/$I`              \
| If `$C` collection, contents of index `$I`
| `$B/$L/d/$C/i/$I#k1-k2`        \
| If `$C` collection, contents of index `$I` anchored at `k1-k2`
| `$B/$L/d/$C/k1/k2/` \
| If `$C` collection `c`, redirect to `../../../i/c.indexes[0]#k1-k2`
