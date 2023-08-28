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

node.Code, node.DispatchTable, node.Function {
    border-color: #422006;
}

node.Code {
    shape: rectangle;
    background-color: #D7983A;
    border-width: 2px;
    border-opacity: 0.5;
}

node.DispatchTable {
    shape: hexagon;
    background-color: #F6DB95;
}

node.Function {
    shape: pentagon;
    background-color: #F7B46F;
}

node.other {
    shape: ellipse;
    background-color: #528A74;
    border-color: #082f49;
}

node.main {
    border-width: 16px;
    border-style: double;
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

edge.other-body, edge.DispatchTable-entry, edge.Function-body, edge.Code-arglist-order {
    line-color: #422006;
    target-arrow-color: #422006;
    color: #422006;
    width: 4px;
}

edge.DispatchTable-nested-closure, edge.Function-default-arg, edge.Code-promise {
    line-color: #3f6212;
    target-arrow-color: #3f6212;
    color: #3f6212;
    width: 4px;
}

edge.Code-push, edge.Code-guard, edge.Code-call {
    line-color: #075985;
    target-arrow-color: #075985;
    color: #075985;
    /** solid for parent-child relationships, 
      * dotted for "far away" (e.g. globals), 
      * dashed for everything else */
    line-style: dashed;
    width: 2px;
}

edge.Code-target {
    line-color: #701a75;
    target-arrow-color: #701a75;
    color: #701a75;
    /** solid for parent-child relationships, 
      * dotted for "far away" (e.g. globals), 
      * dashed for everything else */
    line-style: dashed;
    width: 2px;
}

edge.Code-unknown-extra-pool {
    line-color: #52525b;
    target-arrow-color: #52525b;
    color: #52525b;
    line-style: dashed;
    width: 2px;
}

edge.Code-unexpected-name, edge.Code-unexpected-ast, edge.Code-unexpected-builtin, edge.Code-unexpected {
    line-color: #dc2626;
    target-arrow-color: #dc2626;
    color: #dc2626;
    line-style: dashed;
    width: 4px;
}

edge.far-away {
    line-style: dotted;
    target-arrow-shape: vee;
}
`