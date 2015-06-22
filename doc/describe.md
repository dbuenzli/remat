[HOWTO](howto.html) — DESCRIBE — [REST API](api.html)

# Remat repository description reference v0

This document is the reference manual for repository description files.
It assumes you are familiar with the [HOWTO](howto.html) and JSON.

A Remat repository description defines the contents and configures the
presentation of a repository on the web. It used by the `remat`
command line tool to derive the data needed for the web publication.

A repository description is a directory called `remat-repo` that
contains [JSON text](http://tools.ietf.org/html/rfc4627) files.
This directory is structured as follows:

| File                  | JSON Content         |
|:----------------------|:---------------------|:------------
| `remat-repo/repo.json`        | [`Repo`](#repo) \
| Describes the repository |
| `remat-repo/i/$ID.json` | [`Index`](#index) \
| Describes an index with id `$ID`
| `remat-repo/d/$ID.json`     | [`Doc`](#doc) \
| Describes a document with id `$ID`

All the documents in `remat-repo/d/` belong to the repository.

The formatter, index and document identifiers `$ID` (filename without the
`.json`) can be used in description files to reference the
corresponding objects.

Since formatters, indexes and documents must all be in the same
directory their respective identifiers are unique. Identifiers
differing only in letter casing should be avoided, some file systems
are case insensitive.

## Conventions and basic JSON types {#conventions}

In this document terms like member, member name, value, object, array,
etc. refer to the corresponding JSON concepts. We describe objects by
listing their member name and type. In these descriptions, ?`name`
means that the member `name` of a JSON object can be omitted.

The following basic types are used in description files.

### Type `Integer_range` {#integer_range}

An `Integer_range` is an object that defines a range of integers
from `start` to `end`. If `end` is unspecified the range is right-open.

| Member name    | Type       |
|:---------------|:-----------|:------------
| `start`        | `Integer`  | Range starting integer.
| ?`end`         | `Integer`  | Range ending integer.

### Type `String_map` {#string_map}

A `String_map.<type>` value is an object whose member values are
of type `type`.

### Type `Format_string` {#format_string}

A `Format_string` is `String` with embedded variables of the form
`$(ID)` where `ID` is an arbitrary sequence of characters excluding
commas `,`.

The format string `"$(title) was published in $(year)"` has two
variables `title`

General format for variables is `$(var[,MAP]*(,|)?,[,MAP]*)`.

| Map identifier     | Parsed argument   | Effect
|:-------------------|:------------------|:-------
| case_less    | `String` \
| Default Unicode caseless matching key
| case_lower       | `String`\
| Default Unicode case conversion
| case_upper       | `String` \
| Default Unicode case conversion
| letter_`n`          | `String` \
| Selects the first `n` letters (grapheme clusters).
| date_{Y,YY,YYYY}     | `Date_fuzzy` \
| Selects year in date [see]
| date_{M,MM}     | `Date_fuzzy` \
| Selects month in date [see]
| date_{d,dd}     | `Date_fuzzy` \
| Selects day in date [see]
| date_e     | `Date_fuzzy` \
| Day of week (1-7), monday is 1, 0 if date is partially specified.
| int_{3,4,6,9} | `Integer` \
| For sorting numbers made of up to `n` digits.
| map_`name` | `String` \
| in context publisher defined map with name `name`
| pmap_`name` | `String` \
| in context publisher defined partial map with name `name`

[see]: http://unicode.org/reports/tr35/tr35-dates.html#Date_Format_Patterns

### Type `Uri` {#uri}

An `Uri` value is a `String` representing an absolute or relative
[URI](http://tools.ietf.org/html/rfc3986). Member names of type `Uri`
are often post-fixed by `_href`.

### Type `Lang` {#lang}

A `Lang` value is a `String` that must be BCP47 [language tag][bcp47ltag].

### Type `Doc_id` {#doc_id}

A `Doc_id` is a `String` value. If the string value is `"ID"`, the document
description file `remat-repo/d/ID.json` must exist.

### Type `Index_id` {#index_id}

An `Index_id` is a `String` value. If the string value is `"ID"`, the index
description file `remat-repo/i/ID.json` with an [`Index`](#index) object
*must* exist.

### Type `Doc_type` {#doc_type}

A `Doc_type` value is a `String` defining the digital type of a document.

| Value  | Document type
|:-------|:--------------------
| `"views"`    | Sequence of digital image views (atomic document)
| `"set"`      | Document set

### Type `Doc_views_type` {#doc_views_type}

A `Doc_views_type` value is a `String` defining the type of document
being viewed.

| Value  | Viewed document type
|:-------|:--------------------
| `"monograph"` | A monograph
| `"serial_issue"`  | The issue of a serial
| `"image"`  | An arbitrary image (photograph, postcard, poster, etc.)

### Type `Doc_set_type` {#doc_set_type}

A `Doc_set_type` value is a `String` defining the type of document
sets

| Value  | Document set type
|:-------|:--------------------
| `"collection"` | Arbitrary collection of atomic documents
| `"serial"`     | Serial with issues as atomic documents

### Type `Date` and `Date_fuzzy` {#dates}

A `Date` is a `String` formatted as a
[RFC 3339](http://tools.ietf.org/html/rfc3339) timestamp. A
`Date_fuzzy` is either a `Date` or a string of one of the following
forms:

| Form                    | Constraint
|:------------------------|:------------------
| YYYY-MM-DD              | Y, M, D in [#0-9]
| YYYY-MM                 | Y, M in [#0-9]
| YYYY                    | Y in [#0-9]

The hash `#` means that the information about a particular digit is
not available, it is substituted by a `?` on the web.

`Date` and `Date_fuzzy` values are sorted by lexicographical ASCII
order which leads naturally to chronological order with unavailable
information coming before the rest (this seems better from an user
interaction point of view, for long time spans there's less chance to
miss it). For example `187#` is before `1871`. The order and
presentation in the interface will will be `187?`, `1871`.

### Type `Locv` {#Locv}

[bcp47ltag]: http://tools.ietf.org/html/rfc4646#section-2.1
[bcp47lrange]: http://tools.ietf.org/html/rfc4647#section-2

A `Locv.<type>` is a localized `type` value. It is a
[`String_map.<type>`](#string_map) value whose member names are BCP 47
[basic language ranges][bcp47lrange].

A basic language range is either a [language tag][bcp47ltag] or the
wildcard range `*` that matches any language. If the data is not
localized, the object should just specify a value in a wildcard `"*"`
member.

The following `"label"` member value of type `Locv.<String>` is the
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

### Type `Mem_path` {#mem_path}

A `Mem_path` value specifies the (sub)member of an object using
a path represented by an `Array.<String>`.

The result of a lookup `l` on an object `o` is performed as follows.
`o` is looked up along `l.path`:

```
var vs = o[l.path[0]][l.path[1]]...
```

If `array` is `true`, `vs` must be a an array at that point, if it is
`false` put `vs` in an array. For each value in this array, we convert
it to a string, extract a substring according to `extract` and map the
result by looking up `map`. If `map_partial` is `true` no error is
generated if the lookup fails, the lookup key is returned instead. The
transformed array is returned (or the first element is returned if
`array` is false).

TODO. If the path leads to

* string -> string
* boolean -> converted to a string ("true", "false")
* number -> converted to a string
* array -> array of string/boolean/number
* null, object -> error

### Type `Doc_ref` {#doc_ref}

Given a set of variables looked up in a Document a `Doc_ref` object
describes how to generate a short description and a link to a
document. [`Doc_ref`](api.html#doc_ref) API objects are generated from
a corresponding `Doc_ref` object by applying its formatters to a given
document.


| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `descr` | [`Format_string`](#format_string) \
| Format string for the `descr` field.
| ?`descr_extra` | [`Format_string`](#format_string) \
| Format string for the `descr_extra` field.
| ?`date` | [`Format_string`](#format_string) \
| Format string for the date.
| ?`doc_type` | `Boolean` \
| `true` if the `doc_type` field should be present (absent implies `false`).
| ?`doc_count` | `Boolean` \
| `true` if the `doc_count` field should be present (absent implies `false`).


### Type `Var_envs` {#var_envs}

A `Var_envs` object defines sets of maps from variable to strings.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| ?`file_scan`         | [`Format_string`](#format_string) \
| File path with variables
| ?`ranges`       \
| [`String_map`](#string_map)`.<Array.<`[`Integer_range`](#integer_range)`>>` \
| maps variables to closed ranges
| ?`set`          | [`String_map`](#string_map)`.<Array.<String>>` \
| maps variables to set of strings


Map are determined either (TODO or combination ?) by one of
the `file_scan`, `ranges` or `set` field as follows:

* `file_scan`, the format string must be a file path with
  variables. Each map is defined by matching path segments and
  subsegments to files present on the file system.
  Example: with the file scan `path/to/doc-$(chapter)-$(view).png` and a
  directory `path/to` which holds the files `doc-1-001.png` and
  `doc-2-001.png` the following environments are defined (written as
      [`String_map`](#string_map)) s:
```json
  { "chapter": "1", "view": "001" }
{ "chapter": "2", "view": "002" }
```

* `ranges`, maps variable names to arrays of
  [`Integer_range`](#integer_ranges). The resulting environment
  is the cartesian product of the variables.
* `set`, maps variables to set of strings. Variables take the values
  in the set of strings. For example with the string map
  `{ "view": [ "001", "002" ] }`, the domain of `$(view)` is the
  set of strings `"001"`, `"002"`.

### Type `Ui_link` {#ui_link}

A `Ui_link` object defines an URI presented to the end-user.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `text`       | `String`             \
| The text of the link.
| `href`      | `Uri`                \
| URI of the link.


### Type `Ui_locale` {#ui_locale}

A `Ui_locale` object defines a locale to publish and the end-user URI
prefix under which it is presented.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `locale`       | `String`             \
| A BCP 47 [language tag][bcp47ltag].
| `href`      | `Uri`                \
| URI prefix under which the locale is presented.


## Type `Repo` {#repo}

The `Repo` object defines global parameters for the repository. The
[`Repo`](api.html#repo) API object is generated from the
`Repo` object.

| Member name      | Type                |
|:-----------------|:------------------------|:------------
| `version`        | `Integer`               \
| Remat API version number, currently 0.
| `name`           | `Locv.<String>`      | Repository name.
| `publisher`      | `Locv.<`[`Ui_link`](#ui_link)`>` \
| Link to publisher.
| `ui_locales`        | `Array.<`[`Ui_locale`](#ui_locale)`>` \
| End-user locales.
| `indexes`        | `Array.<`[`Index_id`](#index_id)`>` \
| Repository indexes.
| ?`search_href`   | `Uri`                   | URI for search queries.


The `publisher` member is a localized link shown in the UI to
identify and link to the publisher of the repository.

The `ui_locales` member defines the locales that are made available to
the end-user in the user interface. The process to select the actual
locale shown to the end-user depends on the requested URI, see
[locale selection](api.html#locale-selection) in the API
documentation.

The `indexes` member defines the different indexes by which the
repository can be browsed. The first element of the array is the
default index that is shown.

## Type `Index` {#index}

An `Index` object defines an index for browsing a repository or
a document set.

An index defines a sort key by which document are sorted. Headings
that group the sorted list of document under common labels and sublabels and
finally document references that provide a description and a link to actual
documents.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `name`        | `Locv.<`[`Ui_link`](#ui_link)`>`   \
| Index name and end-user URI under which it is available.
| ?`string_maps` \
| [`String_map`](#string_map)`.<`[`String_map`](#string_map)`.<String>>` \
| string maps used in formatters
| `doc_vars` \
| [`String_map`](#string_map)`.<`[`Mem_path`](#mem_path)`>` \
| document variables declaration
| `key` | `Locv.<`[`Format_string`](#formatter)> \
| Formatter applied on documents generate the index sort key
| `headings`    | `Locv.<Array.<`[`Headings`](#headings)`>>` \
| Index headings levels
| `doc_ref` | `Locv.<`[`Doc_ref`](#doc_ref)`>` \
| The index's document references.

The formatter specified in `key` is used on each document to generate
its sort key for the index in a particular locale. For now these sort
keys are compared using binary lexographical comparison on the string.
Future version will use the unicode collation algorithm according to
the locale. If the sort order of the index is locale independent, use
`"*"` as the locale. If the formatter generates an array of keys on a
document then the document shows up multiple times in the list.

TODO Describe heading generation. Basically the heading formatter
is applied on the *sorted* list doc, while it doesn't change everything
falls under the same header. The procedure is nested.


### `Headings` {#headings}

A `Headings` object defines how to generate the headings of an
index. They define the [`Index_node`](api.html#index_node)
and objects in the API.

| Member name     | Type                |
|:----------------|:--------------------|:------------
| `label` | [`Format_string`](#format_string)   \
| Heading label formatted with the document variables
| ?`label_href` | [`Format_string`](#format_string)   \
| Heading label anchor, formatter applied on the first document that generates a new header, if unspecified `label` is used.
| ?`toc`  | `"all"`, `"multiples"` or `"custom"` \
| Headings toc specification (see below).
| ?`toc_multiple` | `Integer`  | Headings toc multiple
| ?`toc_custom` | `Array.<String>` | Headings labels mentioned in toc.

If `toc` is present a table of contents of the headings is
generated according to the member value:

* `"all"`,  all headings will be in the table of contents.
* `"multiples"`, if labels convert to integers extrema
  are mentioned in the toc aswell as any multiple of the
  number given in `toc_multiple`. If labels
  to not convert to integers, every multiple count label
  in order is mentioned.
* `"custom"` only the heading with labels specified in `toc_custom`
  are mentioned.


## Type `Doc` {#doc}

A `Doc` object defines a document in the repository. It provides
both metadata about the document and references to its media and
derived data files (like OCR data).

Arbitrary metadata member names can be added to a `Doc` object to
allow publishers to define indexes and document references. However
any fields that start with `doc_*` is reserved by remat and should not
be used except with the semantics given in the tables below.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `doc_type`     | [`Doc_type`](#doc_type) | Document type.
| `doc_href`     | `Locv.<`[`Uri`](#uri)`>`   \
| Base end-user URI under the document is available.
| ?`doc_langs`    | `Array.<`[`Lang`](#lang)`>` \
| Languages present in the document.
| `doc_data` | [`Doc_views`](#doc_views) or [`Doc_set`](#doc_set) \
| Data specific to the `doc_type`

## Type `Doc_views` {#doc_views}

A `Doc_views` document defines a document digitized as a sequence
of digital image views.

For planar documents a *view* is one side (recto or verso) of a
folio. Views are sorted according to a publisher defined sort key and
then numbered starting from 1.

Views can be labelled according to various schemes specified in
`view_labels`. Unlabelled views are labelled by their view number.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `type` | [`Doc_views_type`](#doc_views_type) \
| The kind of document that is being viewed.
| `var_envs` | [`Var_envs`](#var_envs) \
| Variable environments to determine the set of views.
| `view_key` | [`Format_string`](#format_string) \
| View sort key.
| ?`thumbs`   | [`Format_string`](#format_string) \
| View thumbnails file paths format.
| ?`images`   | [`Format_string`](#format_string) \
| View thumbnails file paths format.
| ?`ocrs` | [`Format_string`](#format_string) \
| View OCR files (can be a single file for all views).
| `thumbs_href`   | [`Format_string`](#format_string) \
| View thumbnails hrefs format (FIXME relative)
| `images_href`   | [`Format_string`](#format_string) \
| View thumbnails_hrefs format (FIXME relative)
| ?`view_labels`    | `Array.<`[`View_label_range`](#view_label_range)`>` \
| View labels

The `view_env` field defines variable environments used with each of the
subsequent formatters. Each environment is used with the `view_key` formatter,
one view is generated per unique `view_key`.

`view_key` defines the view's sort key, `thumbs` and `thumbs_ref` the
view's thumbnail on the file system and the API reference, `images`
and `images_ref` the view's image on the file system and the API
reference, `ocrs` the OCR data of the view.

If `ocrs` is a single file, the OCR for a view is looked up according
to the view number.

### Type `View_label_range` {#view_label_range}

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `range`        | [`Integer_range`](#integer_range) | View number range
| `labels`       | [`View_labels`](#view_label)      | Labels for this range

### Type `View_labels` {#View_labels}

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `type`         | [`View_label_type`](#view_label_type) | Label type
| ?`custom`      | `String` \
| Custom label (for `custom` kind FIXME devise %d scheme)
| ?`start`       | `Integer` | Starting number (absent: view range start)
| ?`folio`       | `Boolean` | Count folios, not folio sides.

### Type `View_label_type` {#View_label_type}

A `View_label_type` value is a `String` defining the type of labelling.

| Value  | Label kind
|:-------|:--------------------
| `"arabic"` | Arabic numbers 1, 2, 3, 4, etc.
| `"roman"`  | Roman numbers I, II, III, IV, etc.
| `"front_cover"` | Front cover
| `"back_cover"`  | Back cover
| `"custom"` | Custom label specified in the `custom_label` field of

## Type `Doc_set` {#doc_set}

A `Doc_set` is an heterogenous set of *atomic* documents. Documents
can be sorted according to publisher defined indexes.

| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `type` | [`Doc_set_type`](#doc_set_type) \
| Nature of the document set
| `indexes` | `Array.<`[`Index_id`](#index_id)`>` \
| Document set indexes.
| `set` | `Array.<`[`Doc_subset`](#doc_subset)`>` \
| List of document subsets

### Type `Doc_subset` {#doc_subset}

A document subset is part of a document set. It provides a mean to
bulk define documents satisfying an atomic document template. The
mecanism is similar to the way views are determined in
[views documents](#Doc_views).

The `doc_vars` field define the domain of variables that are used by
subsequent formatters. The `doc_key` define unique


| Member name    | Type                 |
|:---------------|:---------------------|:------------
| `var_envs`     | [`Var_envs`](#var_envs) \
| Variables to determine a subset of documents
| `key`     | [`Format_string`](#format_string) \
| Document key for sorting and uniqueness definition
| `labels`     | TODO \
| Something to generate e.g. issue nubmers similar to view labels.
| `doc` | [`Doc`](#doc) \
| An *atomic* document, must not be a [`Doc_set`](#Doc_set).

`key` is used both to sort the documents in the set in order
to label them. The variables in `var_env` binds in `doc`. One
atomic document is generated per `key`.
