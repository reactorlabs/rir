//
// Created by Jakob Hain on 7/28/23.
//

#include "printPrettyGraph.h"
#include "R/r.h"
#include "printRirObject.h"
#include "runtime/rirObjectMagic.h"
#include "utils/HTMLBuilder/HTML.h"

namespace rir {

static inline HTML::Text makeText(PrettyGraphContentPrinter content) {
    std::stringstream s;
    content(s);
    return HTML::Text(s.str());
}

static inline std::string sexpId(SEXP sexp) {
    std::stringstream s;
    s << "0x" << std::hex << (uintptr_t)sexp;
    return s.str();
}

void
printPrettyGraph(SEXP sexp, std::ostream& s, RirObjectPrintStyle style,
                 const std::function<void(PrettyGraphInnerPrinter)>& printInner) {
    auto printPrettyGraphInner = [&]{
        // We do this streaming so we don't have to buffer all the SEXPs in a
        // string. The way we do this is by buffering and writing this object's
        // not-connected HTML last. Then we can just immediately write the
        // connected objects while we're constructing said HTML. In the HTML the
        // final order doesn't matter, just that we don't print one node inside
        // of another.

        auto nodeType = TYPEOF(sexp) == EXTERNALSXP ? rirObjectClassName(sexp) : "other";
        auto node =
            HTML::Div("node")
                .id(sexpId(sexp))
                .cls(std::string("node-") + nodeType);
        printInner({
            [&](auto name){
                node << (HTML::Div("name") << makeText(name));
            },
            [&](auto body) {
                node << (HTML::Div("body") << makeText(body));
            },
            [&](auto connected, auto isChild, auto type, auto description, auto isFarArway) {
                // Print connected object's content and its connecteds directly
                // to the stream, this node's content is still buffered
                printRirObject(connected, s, RirObjectPrintStyle::PrettyGraphInner);

                auto arrow =
                    HTML::Div("arrow")
                        .cls(std::string("arrow-") + nodeType + "-" + type)
                        .addAttribute("data-connected", sexpId(connected))
                    << makeText(description);
                if (isChild) {
                    arrow.addAttribute("data-is-child", "true");
                }
                if (isFarArway) {
                    arrow.cls("arrow-far-away");
                }
                node << std::move(arrow);
            }
        });
        // We've already printed connected objects' HTML nodes, this is the
        // current object's HTML node
        s << node;
    };

    switch (style) {
    case RirObjectPrintStyle::PrettyGraph: {
        // We do this streaming so we don't have to buffer all the SEXPs in a
        // string (see printPrettyGraphInner). However, we also write a static
        // header first, and a static footer after all nodes.
        //
        // One issue is that the header must be static, but we want the main
        // object's name to be the title. Fortunately we can accomplish this via
        // JavaScript.

        // Write header
        s << "<DOCTYPE html><html><head>"
             "<title>RIR</title>"
             "<link rel=\"stylesheet\" href=\"rirPrettyGraph/style.css\">"
             "<script type=\"text/javascript\" src=\"rirPrettyGraph/cytoscape.min.js\"></script>"
             "<script type=\"text/javascript\" src=\"rirPrettyGraph/cytoscape-style.js\"></script>"
             "<script type=\"text/javascript\" src=\"rirPrettyGraph/main.js\"></script>"
             "<script type=\"text/javascript\" src=\"rirPrettyGraph/interaction.js\"></script>"
             "</head><body>"
             "<h1 id=\"sources-needed\">Needs the <code>rirPrettyGraph</code> folder (located in <code>tools</code>) to be in the same location</h1>"
             "<div id=\"js-input\" style=\"display: none\">";

        // Write connected objects' and then main object's HTML nodes
        printPrettyGraphInner();

        // Write footer
        s << "</div></body></html>";
        break;
    }
    case RirObjectPrintStyle::PrettyGraphInner: {
        printPrettyGraphInner();
        break;
    }
    default:
        assert(false && "only PrettyGraph or PrettyGraphInner are allowed");
    }
}

} // namespace rir