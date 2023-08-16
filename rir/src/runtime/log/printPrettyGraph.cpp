//
// Created by Jakob Hain on 7/28/23.
//

#include "printPrettyGraph.h"
#include "R/r.h"
#include "runtime/rirObjectMagic.h"
#include "utils/HTMLBuilder/HTML.h"
#include "utils/HTMLBuilder/escapeHtml.h"
#include <queue>
#include <unordered_set>

namespace rir {

static inline HTML::Text makeText(PrettyGraphContentPrinter content, bool escape = true) {
    std::stringstream s;
    content(s);
    if (escape) {
        return HTML::Text(escapeHtml(s.str()));
    } else {
        return HTML::Text(s.str());
    }
}

static inline std::string sexpId(SEXP sexp) {
    std::stringstream s;
    s << "0x" << std::hex << (uintptr_t)sexp;
    return s.str();
}

void
PrettyGraphInnerPrinter::printUsingImpl(SEXP root,
                                        std::ostream& out,
                                        std::function<void(SEXP sexp, const PrettyGraphInnerPrinter& print)> printImpl) {
    std::unordered_set<SEXP> seen;
    std::queue<SEXP> worklist;
    seen.insert(root);
    worklist.push(root);

    auto printItem = [&](SEXP sexp){
        auto nodeType = TYPEOF(sexp) == EXTERNALSXP ? rirObjectClassName(sexp) : "other";
        auto node =
            HTML::Div("node")
                .id(sexpId(sexp))
                .cls(std::string("node-") + nodeType);
        PrettyGraphInnerPrinter print{
            [&](auto name){
                node << (HTML::Div("name") << makeText(name));
            },
            [&](auto body) {
                node << (HTML::Div("body") << makeText(body, false));
            },
            [&](auto connected, auto isChild, auto type, auto description, auto isFarArway) {
                // Add item to worklist to be printed, unless it was already
                // printed, and add to seen
                // Also, cppcheck can't parse this
                // cppcheck-suppress internalAstError
                if (seen.insert(connected).second) {
                    worklist.push(connected);
                }

                // Print edge to node (buffered)
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
        };
        printImpl(sexp, print);
        // We've already printed connected objects' HTML nodes, this is the
        // current object's HTML node
        out << node;
    };

    // Print header
    out << "<!DOCTYPE html><html lang=\"en\"><head>\n"
           "<title>RIR</title>\n"
           "<link rel=\"stylesheet\" href=\"rirPrettyGraph/style.css\">\n"
           "<script src=\"rirPrettyGraph/dependencies/cytoscape.min.js\"></script>\n"
           "<script src=\"rirPrettyGraph/dependencies/cytoscape-autopan-on-drag.js\"></script>\n"
           "<script src=\"rirPrettyGraph/dependencies/layout-base.js\"></script>\n"
           "<script src=\"rirPrettyGraph/dependencies/cose-base.js\"></script>\n"
           "<script src=\"rirPrettyGraph/dependencies/cytoscape-fcose.js\"></script>\n"
           "<script src=\"rirPrettyGraph/dependencies/cytoscape-lasso.min.js\"></script>\n"
           "</head><body>\n"
           "<h1 id=\"sources-needed\">Needs the <code>rirPrettyGraph</code> folder (located in <code>tools</code>) to be in the same location</h1>\n"
           "<div id=\"js-input\" style=\"display: none\">\n";

    // Print items
    while (!worklist.empty()) {
        printItem(worklist.front());
        worklist.pop();
    }

    // Print footer
    out << "</div>\n"
           "<script type=\"text/javascript\" src=\"rirPrettyGraph/cytoscape-style.js\"></script>\n"
           "<script type=\"text/javascript\" src=\"rirPrettyGraph/utils.js\"></script>\n"
           "<script type=\"text/javascript\" src=\"rirPrettyGraph/main.js\"></script>\n"
           "<script type=\"text/javascript\" src=\"rirPrettyGraph/interaction.js\"></script>\n"
           "</body></html>";
}

} // namespace rir