// Action transform, remove, rename column.
var accion = function(col, acc, val) {
  Shiny.setInputValue("carga_datos_ui_1-accion", [col, acc, val], {priority: "event"});
};