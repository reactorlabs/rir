pkgname <- "grid"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('grid')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Grid")
### * Grid

flush(stderr()); flush(stdout())

### Name: Grid
### Title: Grid Graphics
### Aliases: Grid
### Keywords: dplot

### ** Examples

## Diagram of a simple layout
grid.show.layout(grid.layout(4,2,
                     heights=unit(rep(1, 4),
                                  c("lines", "lines", "lines", "null")),
                     widths=unit(c(1, 1), "inches")))
## Diagram of a sample viewport
grid.show.viewport(viewport(x=0.6, y=0.6,
                            width=unit(1, "inches"), height=unit(1, "inches")))
## A flash plotting example
grid.multipanel(vp=viewport(0.5, 0.5, 0.8, 0.8))
# R: /opt/rir/rir/src/runtime/TypeFeedback.cpp:146: void rir::TypeFeedback::addConnected(rir::ConnectedCollectorOld&) const: Assertion `false && "Feedback should never be hashed (don't call addConnected)"' failed.
