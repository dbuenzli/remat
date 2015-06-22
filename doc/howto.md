HOWTO — [DESCRIBE](describe.html) — [REST API](api.html)

# Remat HOWTO

This quick guide should get you started with Remat. It assumes you
have gone through the `README`, installed the software, have some data
to work on and that your are comfortable with the command line.

## Introduction

Remat publishes a repository. A *repository* contains a set of
documents.  A *document* is either:

* An *atomic document*. An atomic unit of publication: a newspaper issue,
  a book, etc.

* A *document set*. A set of atomic documents that can be sorted by
  custom indexes: the issues of a newspaper, proceedings, serials,
  etc.

The following type of atomic documents are currently supported:

* *views*. A views document is a sequence is a sequence of digital
  image *views*. This can be a monograph, a serial issue or any kind
  of image captured by a digital image.

A document has arbitrary, user defined, metadata under the form of
key-value pairs. Document title, date of publication, authors, etc.

A repository has a shallow hierarchy: an atomic document either
belongs directly to the repository or to a *single* document
set. However the repository and document sets can be sorted and
browsed according to arbitrary custom hierarchical indexes based on
document metadata.

Remat provides two tools to publish digitized documents on the web,
`remat` and `rematd`.

* `remat` allows to organize and process the data resulting from the
  digitization process and produce the requested data for the web
  publication.

* `rematd` is an SCGI daemon (or CGI executable) that works with a
  webserver (nginx, apache, etc.) to provide a search interface to the
  repository.

`rematd` is only needed for searching the repository. If you just want to
provide a browsing interface to your documents, `remat` will generate
a set of static files that you can just publish with any webserver.

Both tools are extensively documented via man pages which are also
available interactively by invoking the tools with `help` :

    remat help
    rematd help


## Describing your repository with description files

The digitization process produces a collection of files of different
types, high-resolution TIFF, PNG or JPEG files, PDFs, OCR data,
etc. `remat` needs to know how these disparate files are related to
form documents, collections and the contents of your archive.

In order to communicate that information to `remat` you need to write
description files. These files can be hand-written, generated from
other description standards, databases, digitization workflows, or
higher-level user interfaces. Description files are text files made of
JSON text ([short introduction](http://json.org)). They must be kept
for evolving the archive's contents however they are not a long-term
archival description format, they are software configuration files.

## Your data

The raw material of your digitization process should be image files,
one per page, possibly facsimile PDFs and OCR data.

Usually these are high-resolution TIFF files which are unsuitable for
web publication. You will need to convert them to JPEG or PNG
files. The various tradeoffs you need to consider in doing so are
beyond the scope of this document. Just make sure you are not
publishing image beyond 500Ko per page. Also try to avoid JPEG as much
as possible as the compression scheme is ill suited for images of text
(ringing artefacts). If your documents are black and white printed
text high-resolution bilevel PNG images are a good choice, they are a
good trade-off between quality and size.

### Problems

* What if I have only dual-layer PDFs ?
   You can try to extract the images with the `pdfimages` tool
   (distributed with `xpdf`). For the OCR however it's going to be
   problematic.

* Talk about physical size, metadata, `exiftool`, black and white.
