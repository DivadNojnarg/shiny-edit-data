# with_tooltip

    Code
      tableEditor:::with_tooltip("tooltip", "value")
    Output
      <abbr style="text-decoration: underline; text-decoration-style: dotted; cursor: help" title="value">tooltip</abbr>

# define_columns_diff works

    Code
      tableEditor:::define_columns_diff(dbReadTable(pool, config_get("db_data_name")))
    Output
      $Species
      $cell
      [1] "\n      function(cellInfo, state) {\n        let isChanged = '';\n        let initVal = initData[cellInfo.column.name][cellInfo.index];\n        if (initVal !== null && initVal !== cellInfo.value) {\n          isChanged = `<span style=\"color: red;\">(old: ${initVal})</span>`;\n        }\n        return `<div>${cellInfo.value} ${isChanged}</div>`\n    }\n  "
      attr(,"class")
      [1] "JS_EVAL"
      
      $html
      [1] TRUE
      
      attr(,"class")
      [1] "colDef"
      

# create_table_cols works

    Code
      tableEditor:::create_table_cols(state)
    Output
      $Species
      $cell
      [1] "\n      function(cellInfo, state) {\n        let isChanged = '';\n        let initVal = initData[cellInfo.column.name][cellInfo.index];\n        if (initVal !== null && initVal !== cellInfo.value) {\n          isChanged = `<span style=\"color: red;\">(old: ${initVal})</span>`;\n        }\n        return `<div>${cellInfo.value} ${isChanged}</div>`\n    }\n  "
      attr(,"class")
      [1] "JS_EVAL"
      
      $html
      [1] TRUE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $id
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $row_names
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $validated
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $locked
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $timestamp
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $Sepal.Length
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $last_updated_by
      $name
      [1] "Last updated by"
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $validate
      $show
      [1] TRUE
      
      $cell
      [1] "function(cellInfo, state) {\n         if (cellInfo.row.status === 'OK') {\n           return null;\n         } else if (cellInfo.row.status === 'IN REVIEW') {\n           return `\n             <div>\n               <button\n                 onclick=\"Shiny.setInputValue('accept-row', ${cellInfo.index + 1}, {priority: 'event'})\"\n                 class='btn btn-success btn-sm'\n               >\n                 <i class=\"fas fa-check\" role=\"presentation\" aria-label=\"check icon\"></i>\n               </button>\n               <button\n                 onclick=\"Shiny.setInputValue('reject-row', ${cellInfo.index + 1}, {priority: 'event'})\"\n                 class='btn btn-danger btn-sm'\n               >\n                 <i class=\"fas fa-xmark\" role=\"presentation\" aria-label=\"xmark icon\"></i>\n               </button>\n             </div>\n           `\n         } else if (cellInfo.row.validated) {\n           return `\n             <button\n               onclick=\"Shiny.setInputValue('reject-row', ${cellInfo.index + 1}, {priority: 'event'})\"\n               class='btn btn-danger btn-sm'\n             >\n               <i class=\"fas fa-xmark\" role=\"presentation\" aria-label=\"xmark icon\"></i>\n             </button>\n           `\n         }\n      }"
      attr(,"class")
      [1] "JS_EVAL"
      
      $header
      <abbr style="text-decoration: underline; text-decoration-style: dotted; cursor: help" title="Validate current row?">validate</abbr>
      
      $html
      [1] TRUE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $status
      $cell
      [1] "function(cellInfo, state) {\n         let colorClass;\n         switch (cellInfo.value) {\n           case 'OK':\n             colorClass = 'bg-secondary';\n             break;\n           case 'IN REVIEW':\n             colorClass = 'bg-warning';\n             break;\n           case 'REJECTED':\n             colorClass = 'bg-danger';\n             break;\n           case 'ACCEPTED':\n             colorClass = 'bg-success';\n             break;\n         }\n        return `<span class=\"badge ${colorClass}\">${cellInfo.value}</span>`\n      }"
      attr(,"class")
      [1] "JS_EVAL"
      
      $html
      [1] TRUE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $Petal.Length
      $cell
      [1] "function(cellInfo, state) { return `<span class=\"badge rounded-pill text-bg-primary\">${cellInfo.value}</span>`; }"
      attr(,"class")
      [1] "JS_EVAL"
      
      $html
      [1] TRUE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      

---

    Code
      tableEditor:::create_table_cols(state)
    Output
      $Species
      $cell
      [1] "\n      function(cellInfo, state) {\n        let isChanged = '';\n        let initVal = initData[cellInfo.column.name][cellInfo.index];\n        if (initVal !== null && initVal !== cellInfo.value) {\n          isChanged = `<span style=\"color: red;\">(old: ${initVal})</span>`;\n        }\n        return `<div>${cellInfo.value} ${isChanged}</div>`\n    }\n  "
      attr(,"class")
      [1] "JS_EVAL"
      
      $html
      [1] TRUE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $id
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $row_names
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $validated
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $locked
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $timestamp
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $Sepal.Length
      $show
      [1] FALSE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $last_updated_by
      $name
      [1] "Last updated by"
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $validate
      $show
      [1] FALSE
      
      $cell
      [1] "function(cellInfo, state) {\n         if (cellInfo.row.status === 'OK') {\n           return null;\n         } else if (cellInfo.row.status === 'IN REVIEW') {\n           return `\n             <div>\n               <button\n                 onclick=\"Shiny.setInputValue('accept-row', ${cellInfo.index + 1}, {priority: 'event'})\"\n                 class='btn btn-success btn-sm'\n               >\n                 <i class=\"fas fa-check\" role=\"presentation\" aria-label=\"check icon\"></i>\n               </button>\n               <button\n                 onclick=\"Shiny.setInputValue('reject-row', ${cellInfo.index + 1}, {priority: 'event'})\"\n                 class='btn btn-danger btn-sm'\n               >\n                 <i class=\"fas fa-xmark\" role=\"presentation\" aria-label=\"xmark icon\"></i>\n               </button>\n             </div>\n           `\n         } else if (cellInfo.row.validated) {\n           return `\n             <button\n               onclick=\"Shiny.setInputValue('reject-row', ${cellInfo.index + 1}, {priority: 'event'})\"\n               class='btn btn-danger btn-sm'\n             >\n               <i class=\"fas fa-xmark\" role=\"presentation\" aria-label=\"xmark icon\"></i>\n             </button>\n           `\n         }\n      }"
      attr(,"class")
      [1] "JS_EVAL"
      
      $header
      <abbr style="text-decoration: underline; text-decoration-style: dotted; cursor: help" title="Validate current row?">validate</abbr>
      
      $html
      [1] TRUE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $status
      $cell
      [1] "function(cellInfo, state) {\n         let colorClass;\n         switch (cellInfo.value) {\n           case 'OK':\n             colorClass = 'bg-secondary';\n             break;\n           case 'IN REVIEW':\n             colorClass = 'bg-warning';\n             break;\n           case 'REJECTED':\n             colorClass = 'bg-danger';\n             break;\n           case 'ACCEPTED':\n             colorClass = 'bg-success';\n             break;\n         }\n        return `<span class=\"badge ${colorClass}\">${cellInfo.value}</span>`\n      }"
      attr(,"class")
      [1] "JS_EVAL"
      
      $html
      [1] TRUE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      
      $Petal.Length
      $cell
      [1] "function(cellInfo, state) { return `<span class=\"badge rounded-pill text-bg-primary\">${cellInfo.value}</span>`; }"
      attr(,"class")
      [1] "JS_EVAL"
      
      $html
      [1] TRUE
      
      $align
      [1] "center"
      
      attr(,"class")
      [1] "colDef"
      

