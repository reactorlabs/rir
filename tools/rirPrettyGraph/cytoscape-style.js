// This is a STYLESHEET in Cytoscape CSS, see https://js.cytoscape.org/#style
// It's a string embedded in JavaScript because that's easiest to load from main.js
// language=CSS
const style = `
node {
    label: data(name);
    compound-sizing-wrt-labels: include;
    text-valign: center;
    text-halign: center;
    font-size: 12px;
    /* Shape and color for misc, rare RIR structures */
    shape: triangle;
    background-color: #41485A;
}

node.node-Code {
    shape: rectangle;
    background-color: #D7983A;
    border-width: 2px;
    border-style: solid;
    border-color: #422006;
    border-opacity: 0.5;
}

node.node-DispatchTable {
    shape: hexagon;
    background-color: #F6DB95;
}

node.node-Function {
    shape: pentagon;
    background-color: #F7B46F;
}

node.node-other {
    shape: ellipse;
    background-color: #528A74;
}

edge {
    label: data(label);
    curve-style: bezier;
    target-arrow-shape: triangle;
    text-rotation: autorotate;
    /* margin-x in case the arrow is vertical, if horizontal it will barely be noticed */
    text-margin-x: -10px;
    text-margin-y: -10px;
    font-size: 10px;
}

edge.arrow-DispatchTable-entry {
    line-color: #422006;
    target-arrow-color: #422006;
    color: #422006;
    width: 4px;
}

edge.arrow-Function-body {
    line-color: #422006;
    target-arrow-color: #422006;
    color: #422006;
    width: 4px;
}

edge.arrow-Code-arglist-order {
    line-color: #422006;
    target-arrow-color: #422006;
    color: #422006;
    width: 2px;
}

edge.arrow-Function-default-arg {
    line-color: #3f6212;
    target-arrow-color: #3f6212;
    color: #3f6212;
    width: 2px;
}

edge.arrow-Code-promise {
    line-color: #3f6212;
    target-arrow-color: #3f6212;
    color: #3f6212;
    width: 2px;
}

edge.arrow-Code-push, edge.arrow-Code-guard, edge.arrow-Code-call {
    line-color: #075985;
    target-arrow-color: #075985;
    color: #075985;
    /** solid for parent-child relationships, 
      * dotted for "far away" (e.g. globals), 
      * dashed for everything else */
    line-style: dashed;
    width: 2px;
}

edge.arrow-Code-unknown-extra-pool {
    line-color: #701a75;
    target-arrow-color: #701a75;
    color: #701a75;
    line-style: dashed;
    width: 4px;
}

edge.arrow-Code-unexpected-name, edge.arrow-Code-unexpected-ast, edge.arrow-Code-unexpected-builtin, edge.arrow-Code-unexpected {
    line-color: #dc2626;
    target-arrow-color: #dc2626;
    color: #dc2626;
    line-style: dashed;
    width: 8px;
}

edge.arrow-far-away {
    line-style: dotted;
    target-arrow-shape: vee;
}
`