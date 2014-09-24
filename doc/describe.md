[HOWTO](howto.html) — DESCRIBE — [REST API](api.html)

# Luigi description files reference v0

This document is the reference manual for repository description files.
It assumes you are familiar with the [HOWTO](howto.html) and JSON.

Luigi description files define the contents and configure the
presentation of a repository on the web. They are used by the `luigi`
command line tool to derive the data needed for the web publication.

Description files are [JSON text](http://tools.ietf.org/html/rfc4627) 
files. They must be placed in a directory called `ldb` according
to the following convention:

| File                  | JSON Content         |           
|:----------------------|:---------------------|:------------
| `ldb/repo.json`        | [`Repo_d`](#type-Repo_d) \
| Describes the repository  |
| `ldb/i/$ID.json` | [`Index_d`](#type-Index_d) \
| Describes an index with id `$ID`
| `ldb/d/$ID.json`     | [`Document_d`](#type-Document_d) \
| Describes a document with id `$ID`

All the documents in `ldb/d/` belong to the repository.

The index and document identifiers `$ID` (filename without the
`.json`) can be used in description files to reference the
corresponding objects.

Since indexes and documents must all be in the same directory their
respective identifiers are unique. Identifiers differing only in
letter casing should be avoided, some file systems are case
insensitive.


## Conventions and basic JSON types {#conventions}

In this document terms like member, member name, value, object, array,
etc. refer to the corresponding JSON concepts. We describe objects by
listing their member name and type. In these descriptions, ?`name`
means that the member `name` of a JSON object can be omitted. 

The following basic types are used in description files.

### `Doc_type` {#type-Doc_type} 

TODO use something user-friendly, use ints for the API.

A `Doc_type` value is an `Integer` defining the type of a document.


| Value  | Document type
|:-------|:--------------------
| 0      | Collection
| 1      | Printed text


### `Locales` {#type-Locales}

[bcp47ltag]: http://tools.ietf.org/html/rfc4646#section-2.1
[bcp47lrange]: http://tools.ietf.org/html/rfc4647#section-2

A `Locales.<type>` value represents localized data of type `type`. It
is an object whose member names are BCP 47
[basic language ranges][bcp47lrange] with corresponding member values
of type `type`.

A basic language range is either a [language tag][bcp47ltag] or the
wildcard range `*` that matches any language. If the data is not
localized, the object should just specify a value in a wildcard `"*"`
member.

The following `"label"` member value of type `Locales.<String>` is the
same for all locales.

```json
{ "label": { "*": "1871" } }
```

Below, there are specific cases for the english and french locale,
other locales will use the wildcard language range.

```json
{ "label": { "*": "03", "en": "March", "fr": "mars" } }
```

And finally, in the following the value is only defined for the
english, french and german locale.

```json
{ "label": { "en": "Title", "fr": "Titre", "de": "Titel" } }
```

### `Uri` {#type-Uri} 

An `Uri` value is a `String` representing an absolute or relative
[URI](http://tools.ietf.org/html/rfc3986). Member names of type `Uri`
are often post-fixed by `_href`.

### `Index_id` {#type-Index_id} 

An `Index_id` is a `String` value. If the string value is `"ID"`, the index
description file `ldb/i/ID.json` *must* exist.

### `Doc_id` {#type-Doc_id} 

A `Doc_id` is a `String` value. If the string value is `"ID"`, the document
description file `ldb/d/ID.json` must exist.

### `Date` and `Partial_date` {#type-Dates}

A `Date` is a `String` formatted as an
[RFC 3339](http://tools.ietf.org/html/rfc3339) timestamp. A
`Partial_date` is either a `Date` or a string of one of the following
forms:

| Form                    | Constraint        
|:------------------------|:------------------
| YYYY-MM-DD              | Y, M, D in [#0-9] 
| YYYY-MM                 | Y, M in [#0-9] 
| YYYY                    | Y in [#0-9] 

The hash `#` means that the information about a particular digit is
not available, it is substituted by a `?` on the web.

`Date` and `Partial_date` values are sorted by lexicographical ASCII
order which leads naturally to chronological order with unavailable
information coming before the rest (this seems better from an user
interaction point of view, for long time spans there's less chance to
miss it). For example `187#` is before `1871`. The order and
presentation in the interface will will be `187?`, `1871`.

### `Formatter` {#type-Formatter}

A `Formatter` is an array of strings and *lookup* objects that
describe members to look in an object. Used on an object, a formatter
defines the string that results by concatenating the strings of the
array with those that result from looking up to object according to
lookup objects.

If in a formatter a lookup object has its `"array"` member set to
`true`. The lookup can point to an array of values and applying the
formatter to an object results in an array of strings.

The following formatter, looks up the `"tags"` member of objects and
generates a string for each tag it finds:

```
["The tag is: ", { "path": ["tags"], "array": true" }, "." ]
```

Applied to the object `{ "tags": ["hey", "ho"] }` it generates the
string array:

```
[ "The tag is: hey.", "The tag is: ho." ]
```

Formally a `Formatter` is an `Array<String or Lookup>`. 

### `Lookup` object {#type-Lookup}

A `Lookup` is an object with the following members


| Member name     | Type                | 
|:----------------|:--------------------|:------------
| `path`        | `Array.<String>`            \
| Member name chain to lookup.
| ?`array`        | `Boolean` \
| Generate array of lookup values (defauts to `false`)
| ?`sub`      | `Substring`        \
| Substring of the lookup value to extract
| ?`map` | `Object.<String>` \
| Maps the lookup value (default maps any value to itself).
| ?`map_partial` | `Boolean` \
| Specifies if the map is partial (defaults to `false`)

The `Substring` type is a string that must be one of the following
values.

| Subfield extractor | Member value type | Part extracted
|:-------------------|:------------------|:-------
| `"Y"`, `"YY"`, `"YYYY"`  | `Partial_date`    | Year
| `"M"`, `"MM"`          | `Partial_date`    | Month (0-12), [see].
| `"d"`, `"dd"`          | `Partial_date`    | Day of month, [see].
| `"e"`                | `Partial_date`    \
| Day of week (1-7), monday is 1.
| `"."`, `".."`, ...                | `String` \
| First, first two, etc. characters 
| TODO regexp support ?     |  `String` |

Day of week returns 0 if the date is only partially specified.

[see]: http://unicode.org/reports/tr35/#Date_Format_Patterns

The result of a lookup `l` on an object `o` is performed as follows.
`o` is looked up along `l.path`:

```
var vs = o[l.path[0]][l.path[1]]...
```

If `array` is `true`, `vs` must be a an array at that point, if it is
`false` put `vs` in an array. For each value in this array, we convert
it to a string, extract a substring according to `sub` and map the
result by looking up `map`. If `map_partial` is `true` no error is
generated if the lookup fails, the lookup key is returned instead. The
transformed array is returned (or the first element is returned if
`array` is false).

The following example is a formatter that formats a
 [`PartialDate`](#type-Dates) value of the `date` member of an object
 as an abreviated month name:

```
[ "The month is: ", 
  { "path": ["date"], 
    "substring": "MM", 
    "map": { "00": "???", "01": "Jan", "02": "Feb", "03": "Mar", 
             "04": "Apr", "05": "May", "06": "Jun", "07": "Jul", 
             "08": "Aug", "09": "Sep", "10": "Oct", "11": "Nov", 
             "12": "Dec" } ]
```

## `Repo_d` object {#type-Repo_d}

The `Repo_d` object defines global parameters for the repository. The
[`Repo`](api.html#type-Repo) API object is generated from the
`Repo_d` object.

| Member name      | Type                | 
|:-----------------|:------------------------|:------------
| `version`        | `Integer`               \
| Luigi API version number, currently 0.
| `locales`        | `Array.<`[`Locale_d`](#type-Locale_d)`>` \
| End-user locales.
| `name`           | `Locales.<String>`      | Repository name.
| `indexes`        | `Array.<`[`Index_id`](#type-Index_id)`>` \
| Repository indexes.
| ?`search_href`   | `Uri`                   | URI for search queries.
| `publisher`      | `Locales.<String>`      | Name of the repository publisher
| `publisher_href` | `Locales.<Uri>`         | Link to publisher webpage


The `locales` member defines the locales that are made available to
the end-user with [`Locale_d`](#type-Locale_d) objects. The process to
select the actual locale shown to the end-user depends on the
requested end-user URI, see
[locale selection](API.html#locale-selection) in the API
documentation.

The `indexes` member defines the indexes for the repository. The first
element of the array is the default index that is shown. 

The `publisher` member is a localized string shown in the UI to
identify the publisher of the repository. This string is hyperlinked with
`publisher_href`.


### `Locale_d` object {#type-Locale_d}

A `Locale_d` object defines a locale to publish and the end-user URI
prefix under which it is presented.

| Member name    | Type                 | 
|:---------------|:---------------------|:------------
| `locale`       | `String`             \
| A BCP 47 [language tag][bcp47ltag].
| `ui_href`      | `Uri`                \
| URI prefix under which the locale is presented.



## `Index_d` object {#type-Index_d} 

An `Index_d` object defines an index for browsing a sequence of
documents. When used by the repository or by a collection an
[`Index`](api.html#type-Index) API object is generated from the
`Index_d` object and the sequence of documents.

| Member name    | Type                 | 
|:---------------|:---------------------|:------------
| `label`        | `Locales.<String>`   \
| Index label, formatter applies on corresponding repository or collection
| `ui_href`      | `String`             | Index User URI
| `sort_key` | `Locales.<`[`Formatter`](#type-Formatter)> \
| Document description members to lookup to generate the sort key
| `synopses`     | `"isbd"` or `Locales.<`[`Formatter`](#type-Formatter)> \
| Document synopses, formatter applies on document description
| ?`headings`    | `Array.<`[`Headings_d`](#type-Headings_d)`>` \
| Description of index headings levels

`synopses` if `"isbd"` looks up the document description for an
`"isbd"` member or tries a best-effort ISBD description, from standard
document description members.

The formatter of `sort_key` is used on each document to generate its
sort key for the index in a particular locale. For now these sort key
are compared using binary comparison on the string, future version
will implement use the unicode collation algorithm according to the
locale. If the sort order of the index is locale independent, use
`"*"` as the locale.

TODO Describe heading generation

### `Headings_d` {#type-Headings_d}

A `Headings_d` object defines how to generate a heading level of an
index. 

| Member name     | Type                | 
|:----------------|:--------------------|:------------
| `label` | `Locales.<`[`Formatter`](#type-Formatter)>   \
| Heading label, formatter applies on document description
| ?`toc`  | `"all"`, `"multiples"` or `Array.<String>` \
| Headings toc specification (see below).
| ?`toc_multiple` | `Integer`  | Headings toc multiple

If `toc` is present a table of contents of the headings is
generated according to the member value:

* `"all"`,  all headings will be in the table of contents. 
* `"multiples"`, if labels convert to integers extrema
  are mentioned in the toc aswell as any multiple of the 
  number given in `toc_multiple`. If labels
  to not convert to integers, every multiple count label
  in order is mentioned.
* Array of labels, only the headings with these labels are 
  mentioned.

## `Document_d` object {#type-Document_d}


TODO 
----

* [json-schema](http://json-schema.org/latest/json-schema-core.html)
descriptions. Can make description file edition user-friendlier for cheap
e.g. with this [editor](http://exavolt.github.com/onde/).

* Describe search.json that configures search. 
* Add fields for ocr data in Printed documents
