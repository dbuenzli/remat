Remat — Publish digitized documents on the web
-------------------------------------------------------------------------------
Release %%VERSION%%

Remat is a set of tools for publishing digitized documents on the web.

On the server side Remat publishes a repository of documents as a set
of static image and JSON files. This set of files implement a REST API
to browse and consult the documents of the repository. Search is
handled by a seperate server process that can run as a standalone HTTP
server, CGI process or SCGI process. This architecture allows to run
the system with low server requirements in shared hosting
environments.

For consulting the document repository a client-side web application
is provided that interacts with the server through the REST API.

Remat is distributed under a BSD3 license.
  
Home page: http://erratique.ch/software/remat  
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`


## Features 

TODO


## Installation

Remat can be installed with `opam`:

    opam install remat

## Documentation

There are a few documents in the `doc` directory:

* [howto.md](doc/howto.md), guides you through a publication of a document.
* [describe.md](doc/describe.md), documents the Remat description files 
  used to generate the API of a repository.
* [API.md](doc/API.md), documents the interface between the web server and the
  client. If you want to develop an alternate client or server. 

Besides the tools are self documented type :
    
    remat help
    rematd help

for more help and reference documentation. 

## Current limitations and future work

The first release of Remat is intended to support a first digitization
effort carried at the [CIRA](http://www.cira.ch). The following
limitations could be lifted in future versions of the
software. Funding is welcome.

### Important

- Supports only the FineReader 6v1 OCR format.
  Possible additions: ALTO, hOCR, more recent FineReader formats.
- Deals only with two image sizes thumbnails and full resolution. No
  tiling. Not a problem for bi-level images but that is not suitable for all 
  corpora.
- No double spread presentation.
- Taming larger corpora. Caching mechanisms to avoid rebuilding
  everything when documents are added.
- Support for OAI-PMH requests.
- Support for METS/MODS record publication.
- Workflow improvements. Single workflow from TIFF originals + XML OCR data.
  Including PDF generation.
- Integration with other existing records databases. Procedure to generate 
  the needed remat configuration files.
- Make the pages browsable by robots.
- Improve mobile compatibility. Problem: for now it seems tablets 
  do not support big images on the web. 

### Feature creep

- Use HTML5 offline mode to keep a limited set of docs with you. 
