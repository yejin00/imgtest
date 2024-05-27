# devtools::install_github("sblabkribb/sblims/SBLIMS.auth", auth_token = "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")

library(SBLIMS.auth)
library(shiny)
library(DT)
library(jsonlite)
library(mongolite)
library(tidyverse)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(future)
library(promises)
library(ggplot2)
library(cowplot)
library(magick)
library(grid)
library(png)
library(dplyr)
library(rmarkdown)
library(shinyAce)
library(base64enc)


# sessionInfo()
# packageVersion("grid")
# 임시 디렉토리
# Sys.setenv(TMPDIR = "C:/temp")
# renv::activate()
# renv::restore(lockfile = "renv231114.lock")




# taskDB_init$find()
# taskFiles_init$find()
# upDB_init$find()

# Table 행 안에 버튼 넣기
# js와 taskID, wfKey, state, upKey interaction


# workflow btn
# id:1_inprogress_wf1
#' Title
#'
#' @param taskID
#' @param wfKey
#' @param state
#'
#' @return
#'
#' @examples
create_btns_wf_fu <- function(taskID, wfKey, state) {
  # 버튼 id 생성
  button_ids <- paste0(taskID, "_", c("done", "inprogress", "notstarted"), "_", wfKey)

  paste0(
    '<button class="btn btn-default action-button btn-sm action_button ',
    ifelse(state == 2, "done-button", ""),
    '" id="', button_ids[1],
    '" type="button" onclick=changeButtonColor_wf(this.id) onmousedown="event.preventDefault(); event.stopPropagation(); return false;">Done</button>',

    '<button class="btn btn-default action-button btn-sm action_button ',
    ifelse(state == 1, "in-progress-button", ""),
    '" id="', button_ids[2],
    '" type="button" onclick=changeButtonColor_wf(this.id) onmousedown="event.preventDefault(); event.stopPropagation(); return false;">In Progress</button>',

    '<button class="btn btn-default action-button btn-sm action_button ',
    ifelse(state == 0, "not-started-button", ""),
    '" id="', button_ids[3],
    '" type="button" onclick=changeButtonColor_wf(this.id) onmousedown="event.preventDefault(); event.stopPropagation(); return false;">Not Started</button>'
  )
}


# unit process btn
#id:wf1_done_up4_up2
#' Title
#'
#' @param wfKey
#' @param upName
#' @param state
#' @param upKey
#'
#' @return
#'
#' @examples
create_btns_up_fu <- function(wfKey, upName, state, upKey) {
  # 버튼 id 생성s
  button_ids <- paste0(wfKey, "_", c("done", "inprogress", "notstarted"), "_", upName, "_", upKey)
  paste0(
    '<button class="btn btn-default action-button btn-sm action_button ',
    ifelse(state == 2, "done-button", ""),
    '" id="', button_ids[1],
    '" type="button" onclick=changeButtonColor_up(this.id) onmousedown="event.preventDefault(); event.stopPropagation(); return false;">Done</button>',

    '<button class="btn btn-default action-button btn-sm action_button ',
    ifelse(state == 1, "in-progress-button", ""),
    '" id="', button_ids[2],
    '" type="button" onclick=changeButtonColor_up(this.id) onmousedown="event.preventDefault(); event.stopPropagation(); return false;">In Progress</button>',

    '<button class="btn btn-default action-button btn-sm action_button ',
    ifelse(state == 0, "not-started-button", ""),
    '" id="', button_ids[3],
    '" type="button" onclick=changeButtonColor_up(this.id) onmousedown="event.preventDefault(); event.stopPropagation(); return false;">Not Started</button>'
  )
}




### UI ###

ui <- dashboardPage(skin="black",
                    dashboardHeader(
                      title = "SBLIMS - Shiny Apps",
                      titleWidth = 600
                    ),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(useShinyjs(),
                                  tags$head(
                                    tags$style(HTML("
                                      .done-button {
                                        background-color: green;
                                        color: white;
                                      }
                                      .in-progress-button {
                                        background-color: orange;
                                        color: white;
                                      }
                                      .not-started-button {
                                        background-color: red;
                                        color: white;
                                      }
                                      .main-sidebar {background-color: white !important;}
                                    ")),
                                    tags$script(HTML("
                                      document.addEventListener('DOMContentLoaded', function() {
                                        document.getElementById('editor').addEventListener('paste', function(event) {
                                          const items = (event.clipboardData || event.originalEvent.clipboardData).items;
                                          for (let i = 0; i < items.length; i++) {
                                            if (items[i].type.indexOf('image') !== -1) {
                                              const file = items[i].getAsFile();
                                              const reader = new FileReader();
                                              reader.onload = function(event) {
                                                const base64Data = event.target.result.split(',')[1];
                                                Shiny.setInputValue('clipboard_image', base64Data, {priority: 'event'});
                                              };
                                              reader.readAsDataURL(file);
                                            }
                                          }
                                        });
                                      });
                                    ")),
                                    # 커서 있는 쪽에 이미지 삽입
                                    tags$head(
                                      tags$script(HTML("
                                      Shiny.addCustomMessageHandler('insertImage', function(message) {
                                        var editor = ace.edit('editor');
                                        var position = editor.getCursorPosition();
                                        var newValue = editor.getValue().slice(0, editor.session.doc.positionToIndex(position)) +
                                                       '\\n![Image](' + message.url + ')\\n' +
                                                       editor.getValue().slice(editor.session.doc.positionToIndex(position));
                                        editor.setValue(newValue, -1);
                                      });
                                    "))
                                    )

                                  ),
                                  fluidRow(style = 'padding: 10px;',
                                           div(class="col-sm-1",
                                               div(
                                                 style = "width: 100%; margin-left: 20px; padding: 20px; margin-right:20px;",
                                                 tags$b(
                                                   style = "font-size: 2em; color: black;",
                                                   "TASK"
                                                 ),
                                                 hr(),
                                                 DT::DTOutput(outputId = "task_DBtable", width = "100%")
                                               )
                                           ),
                                           div(class="col-sm-2",
                                               div(
                                                 style = "width: 100%; margin-left: 100px; padding: 20px;",
                                                 tags$b(
                                                   style = "font-size: 2em;",
                                                   "WORKFLOW"
                                                 ),
                                                 hr(),
                                                 DT::DTOutput(outputId = "wf_list", width = "100%")
                                               ),
                                               div(style = "width: 100%; margin-left: 100px; padding: 20px;",
                                                   div(
                                                     br(),
                                                     br(),
                                                     div(
                                                       div(
                                                         tags$br(),
                                                         tags$b(
                                                           style = "font-size: 2em;",
                                                           "UNIT PROCESS"
                                                         ),
                                                         DT::DTOutput(outputId = "up_table"),
                                                         br(),
                                                         br(),
                                                         br()
                                                       )
                                                     )
                                                   )
                                               ),
                                               br(),
                                               br()
                                           ),
                                           div(class="col-sm-7", style="float: right; margin-right: 10px",
                                               tags$b(
                                                 style = "font-size: 2em;",
                                                 "Markdown Editor and Viewer"
                                               ),
                                               br(),
                                               fluidRow(
                                                 div(
                                                   style = "display: flex; justify-content: space-between;",
                                                   div(
                                                     style = "flex-basis:48%;",
                                                     fileInput('file', 'Choose Markdown or R Markdown file',
                                                               accept = c(".md", ".Rmd")),
                                                     downloadButton("saveBtn", "Save Changes"),
                                                     numericInput("height", "Window Height", value = 400, min = 100, step = 10)
                                                   )
                                                 )
                                               ),
                                               br(),
                                               h3("Viewer: Rendering Page"),
                                               fluidRow(style = "overflow-y: auto; border: 1.5px solid #ccc;", id="renderingDiv",
                                                        htmlOutput("output")
                                               ),
                                               br(),
                                               h3("Editor"),
                                               fluidRow(style = "overflow-y: auto;", id="editorDiv",
                                                        aceEditor("editor", mode="markdown", theme="github")
                                               )
                                           )
                                  ),
                                  div(
                                    style = "float: left;",
                                    bsButton("pause", "pause" ),
                                    bsButton("restart", "restart" ),
                                    bsButton("complete", "complete" ),
                                    bsButton("stopped", "stopped" )
                                  )
                    )
)

### SERVER ###

#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#'
#' @examples
server <- function(input, output, session) {

  # mongoDB 연결
  # taskDB_init <- mongolite::mongo(collection="test_taskdb2", db="SBLIMS", url=paste0(Sys.getenv("MONGODB_URL"), "/SBLIMS")) #test db
  taskDB_init <- mongolite::mongo(collection="taskDB_init", db="SBLIMS", url=paste0(Sys.getenv("MONGODB_URL"), "/SBLIMS")) # 공유 db
  taskFiles_init <- mongolite::gridfs(url=paste0(Sys.getenv("MONGODB_URL"), "/SBLIMS"), prefix = 'Task')
  upDB_init <- mongolite::mongo(collection="upDB_init", db="SBLIMS", url=paste0(Sys.getenv("MONGODB_URL"), "/SBLIMS"))


  task_list <- taskDB_init$find()

  # df_tasklist -> reactive value로 할당
  rv <- shiny::reactiveValues(
    df_tasklist = task_list
  )

  output$task_DBtable <- DT::renderDataTable({
    datatable(subset(rv$df_tasklist)[,c('taskID', 'researcher')],
              rownames = FALSE, escape = FALSE, selection = "single", options = list(stateSave = TRUE),
    )
  }, server=FALSE)


  # task -> wf selected output wf_list table

  # current_task_id_num을 반환 -> reactive value
  current_task_id_num <- reactive({
    req(input$task_DBtable_rows_selected)
    if (!is.null(input$task_DBtable_rows_selected)) {
      selected_row <- input$task_DBtable_rows_selected[1]
      # 선택된 첫 번째 행의 id 값 할당
      task_id_num <- rv$df_tasklist[selected_row, "id"]
      return(task_id_num)
    } else {
      return(NULL)
    }
  })

  # current_task_id_num() id 반환 확인
  # observe({
  #   print(current_task_id_num()))
  # })


  # current_task_id_num에 해당하는 wf_list
  wf_list <- reactive({
    req(input$task_DBtable_rows_selected)

    selected_task <- taskDB_init$find(paste('{"id": ', current_task_id_num(), '}', sep = ''))
    structure_field <- selected_task$structure

    #wfKey -> names(structure_field)의 각 원소
    wf_data_filtered_list <- lapply(names(structure_field), function(wfKey) {
      wf <- structure_field[[wfKey]] # 특정 wfKey 반환

      # wfFiles 비었을때
      wfFiles <- wf[["wfFiles"]]
      if (length(wfFiles) == 0) {
        wfFiles <- ""
      }
      else {
        wfFiles <- sapply(wfFiles, function(x) x$filename) #filename만 추출
        wfFiles <- paste(unlist(wfFiles), collapse = ", ") #여러개일 때 paste
      }

      data.frame(
        wfKey = wfKey,
        wfID = unlist(wf["wfID"]),
        # wfTextinput = unlist(wf["wfTextinput"]),
        wfFiles = unlist(wfFiles),
        # wfState = unlist(wf["wfState"])[1],
        wfButtons = create_btns_wf_fu(current_task_id_num(), wfKey, unlist(wf["wfState"])[1])
        # create_btns_wf_fu 함수 인자값 전달
      )
    })
    # rbind 함수를 wf_data_filtered_list의 각 요소에 적용
    wf_list <- do.call(rbind, wf_data_filtered_list)

    return(wf_list)

  })

  #wfKey값 반환
  wf_ids <- reactive({
    req(input$task_DBtable_rows_selected)

    if (!is.null(input$task_DBtable_rows_selected)) {
      selected_row <- input$task_DBtable_rows_selected[1]
      selected_task_id <- rv$df_tasklist[selected_row, "taskID"]
      selected_task <- taskDB_init$find(paste('{"taskID": "', selected_task_id, '"}', sep = ''))
      structure_field <- selected_task$structure
      wf_names <- names(structure_field)
      return(wf_names)
    } else {
      return(NULL)
    }
  })


  # observe({
  #   print(wf_ids())
  # })


  output$wf_list <- DT::renderDataTable({
    datatable(
      wf_list(),
      rownames = FALSE,
      class = 'cell-border stripe',
      escape = FALSE,
      selection = "single"
    )
  })



  # wf -> up selected output wf_list table

  selected_wf_id <- reactiveVal()

  # observe({
  #   print(selected_wf_id())
  # })

  # task table - selected_row 반환
  selected_row <- reactive({
    req(input$task_DBtable_rows_selected)
    selected_row <- input$task_DBtable_rows_selected[1]
    return(selected_row)
  })


  # wf_list table - slected_rows에 해당하는 wfKey 반환 / wf_ids와 F차이 있음
  observeEvent(input$wf_list_rows_selected, {
    selected_rows <- input$wf_list_rows_selected
    if (length(selected_rows) > 0) {
      selected_wf_id(wf_list()[selected_rows, "wfKey"])
    }
  })


  # make up_data_filtered reactive
  up_data_filtered <- reactive({

    # selected_wf_id()의 반환값이 없거나 결측치인 경우에 NULL을 반환
    if (is.null(selected_wf_id()) || is.na(selected_wf_id())) {
      return(NULL)
    }

    req(selected_wf_id())
    # selected_wf_id() 한 번 불러온 reactive value 값을 변수에 저장 -> 재사용 가능
    selected_wf_key <- selected_wf_id()

    selected_task <- taskDB_init$find(paste('{"taskID": "', rv$df_tasklist[selected_row(), "taskID"], '"}', sep = ''))
    selected_wf <- selected_task$structure[[selected_wf_key]]

    if (is.list(selected_wf)) {
      up_data_filtered_list <- lapply(names(selected_wf), function(upKey) {
        up <- selected_wf[[upKey]] # names(selected_wf)의 특정 upKey 대입

        if (is.list(up) && "upID" %in% names(up)) {
          data.frame(
            upKey = upKey,
            upID = unlist(up["upID"]),
            # upTextinput = unlist(up["upTextinput"]),
            # upState = unlist(up["upState"])[1],
            upButtons = create_btns_up_fu(selected_wf_key, unlist(up["upID"]), unlist(up["upState"])[1], upKey)
          )
        } else {
          NULL
        }
      })
      up_data_filtered_list <- up_data_filtered_list[!sapply(up_data_filtered_list, is.null)]
      up_data_filtered <- do.call(rbind, up_data_filtered_list)
      return(up_data_filtered)
    } else {
      return(NULL)
    }
  })


  output$up_table <- renderDT({
    datatable(
      up_data_filtered(),
      rownames = FALSE,
      class = 'cell-border stripe',
      escape = FALSE,
      selection = "single"
    )
  })


  wf_doc <- reactiveVal()

  # id기준으로 변경사항 생기면 db 재검색
  observe({
    req(current_task_id_num(), selected_wf_id())
    result <- taskDB_init$find(paste('{"id":', current_task_id_num(),'}', sep = ''))
    wf_doc(result)
  })


  output$wf_note <- DT::renderDataTable({
    wfTextinput <- lapply(wf_doc()$structure[[selected_wf_id()]]$wfTextinput,
                          function(x) if (is.list(x) && "timestamp" %in% names(x) && "note" %in% names(x)) return(x)
                          else return(NULL))
    wfTextinput <- compact(wfTextinput) # null 값 제외

    if (length(wfTextinput) > 0) {
      df_text <- do.call(rbind, lapply(wfTextinput, function(x) data.frame(Time = unlist(x$timestamp), Note = unlist(x$note))))
      return(df_text)
    } else {
      return(NULL)
    }
  })



  # wf note 입력 db 저장
  observeEvent(input$note_save, {
    if (!is.null(selected_wf_id())) {
      if (!is.null(input$note)){

        #노트 format list로 변경
        new_note <- list(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M"),
          note = input$note
        )

        update_cmd <- list(
          '$push' = setNames(list(new_note), paste0('structure.', selected_wf_id(), '.wfTextinput'))
        )

        if (!is.null(current_task_id_num())) {
          result <- taskDB_init$update(query = paste0('{"id":', current_task_id_num(),'}'), update=jsonlite::toJSON(update_cmd))

          # 입력 초기화 후 활성화
          updateTextInput(session, "note", value = "")
          # invalidateLater(1000, session)

          #바로 update
          wf_doc(taskDB_init$find(paste('{"id":', current_task_id_num(),'}', sep = '')))
        } else {
          showModal(modalDialog(
            title = "Warning",
            "Please select a task first."
          ))
        }
      }
    } else {
      showModal(modalDialog(
        title = "Warning",
        "Please select a workflow first."
      ))
    }
  })


  # up note

  # selected up key 반환
  selected_up_id <- reactiveVal()

  # 단일 up_key 반환
  observeEvent(input$up_table_rows_selected, {
    selected_rows <- input$up_table_rows_selected
    if (length(selected_rows) > 0) {
      selected_up_id(up_data_filtered()[selected_rows, "upKey"])
    }
  })

  observeEvent(input$upnote_save, {
    if (!is.null(selected_wf_id())){
      if (!is.null(input$upnote)) {

        #노트 format list로 변경
        new_note <- list(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M"),
          note = input$upnote
        )
        update_cmd <- list(
          '$push' = setNames(list(new_note), paste0('structure.', selected_wf_id(), '.', selected_up_id(), '.upTextinput'))
        )
        if (!is.null(current_task_id_num())) {
          result <- taskDB_init$update(query = paste0('{"id":', current_task_id_num(),'}'), update=jsonlite::toJSON(update_cmd))

          # 입력 초기화 후 활성화
          updateTextInput(session, "upnote", value = "")
          wf_doc(taskDB_init$find(paste('{"id":', current_task_id_num(),'}', sep = '')))
        } else {
          showModal(modalDialog(
            title = "Warning",
            "Please select a task first."
          ))
        }
      }
    } else {
      showModal(modalDialog(
        title = "Warning",
        "Please select a workflow first."
      ))
    }
  })


  output$up_note <- DT::renderDataTable({

    selected_up_id <- selected_up_id()
    selected_wf_id <- selected_wf_id()


    # 선택된 up_id 및 wf_id가 존재하는지 먼저 확인
    if(!is.null(selected_wf_id) & length(selected_wf_id) > 0 & !is.null(selected_up_id) & length(selected_up_id) > 0){

      if (!is.null(wf_doc())) {
        # 각 리스트를 받아와서 if문으로 조건 검사
        upTextinput <- lapply(wf_doc()$structure[[selected_wf_id]][[selected_up_id]]$upTextinput,
                              function(x) if (is.list(x) && "timestamp" %in% names(x) && "note" %in% names(x)) return(x)
                              else return(NULL))
        if (length(upTextinput) > 0) {
          df_text2 <- do.call(rbind, lapply(upTextinput, function(x) data.frame(Time = unlist(x$timestamp), Note = unlist(x$note))))
          return(df_text2)
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })


  # state js - db update

  observe({
    # input은 ui에서 사용자의 입력을 저장하는 객체이고, 수행한 동작에 따라 값이 변경됨.
    lapply(names(input), function(name) {
      # newState_"로 시작하는 값이 입력되면
      if (grepl("^newState_", name)) {
        state_info <- strsplit(name, "_")[[1]]
        state <- state_info[2]
        task_id <- as.integer(state_info[3])
        wf_key <- state_info[4]

        # 입력값이 변경될 때 마다 observeEvent
        observeEvent(input[[name]], {
          selected_task_id <- rv$df_tasklist[input$task_DBtable_rows_selected[1], "id"]

          query_check_field_exists = sprintf('{"id": %s}', selected_task_id)
          document = taskDB_init$find(query_check_field_exists)

          if (!is.null(document$structure[[wf_key]])) {
            query_update <- sprintf('{"id": %s}', selected_task_id)

            #state 0,1,2로
            update_state_num <- match(state, c("notstarted", "inprogress", "done")) - 1
            update <- paste0('{"$set": {"structure.', wf_key, '.wfState": [', update_state_num ,']}}')

            taskDB_init$update(query_update, update)
          } else {
            print(paste("Field", wf_key, "does not exist in the document with id =", selected_task_id))
          }
        })
      }
    })
  })


  #중첩list
  # .으로 구분된 키를 입력받고 . 기준으로 분리한 후, 해당 키를 찾음.
  find_key <- function(list, key) {
    parts <- strsplit(key, "\\.")[[1]]

    for (part in parts) {
      if (!is.null(list[[part]])) {
        list <- list[[part]]
      } else {
        return(NULL)
      }
    }
    return(list)
  }


  observe({
    lapply(names(input), function(name) {
      if (grepl("^upState_", name)) {

        # upState_done_wf1_up4_up2 js와 이렇게 통신함. upKey는 5번째
        state_info <- strsplit(name, "_")[[1]]
        state <- state_info[2]
        if (length(state_info) >= 5) {
          wf_key <- paste(state_info[3], state_info[5], "upState", sep = ".")
          upKey <- state_info[5]

          observeEvent(input[[name]], {
            selected_task_id <- rv$df_tasklist[input$task_DBtable_rows_selected[1], "id"]

            query_check_field_exists = sprintf('{"id": %s}', selected_task_id)
            document = taskDB_init$find(query_check_field_exists)

            if (!is.null(find_key(document$structure, wf_key))) {
              query_update <- sprintf('{"id": %s}', selected_task_id)

              update_state_num <- match(state, c("notstarted", "inprogress", "done")) - 1
              update <- paste0('{"$set": {"structure.', wf_key, '": [', update_state_num ,']}}')

              taskDB_init$update(query_update, update)
            } else {
              print(paste("Field", wf_key, "does not exist in the document with id =", selected_task_id))
            }
          })
        }
      }
    })
  })

  loaded_data <- reactiveVal(NULL)

  up_data <- modalDialog(
    title = "File",
    fileInput(inputId="upload", label = "Upload"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK"))
  )
  observeEvent(input$newfile, {
    showModal(up_data) # newfile 버튼 누르면 modal 팝업
  })


  # upload

  observeEvent(input$upload, {
    if (!is.null(selected_wf_id())) {
      # print(selected_wf_id())
      # 공백 있을 시, _로 대체
      change_file_name <- gsub(" ", "_", input$upload$name)

      # 파일을 GridFS에 업로드
      file_id = taskFiles_init$upload(input$upload$datapath, change_file_name)

      # 업로드된 파일의 file_id, filename 리스트 넣기
      new_object = list(file_id=file_id$id, filename=change_file_name)

      # selected_wf_id에 new_object 넣어서 push 하는 쿼리
      update_cmd <- list(
        '$push' = setNames(list(new_object), paste0('structure.', selected_wf_id(), '.wfFiles'))
      )

      #  update_cmd로 update
      if (!is.null(current_task_id_num())) {
        result <- taskDB_init$update(query = paste0('{"id":', current_task_id_num(),'}'), update=jsonlite::toJSON(update_cmd))
      }
    } else {
      showModal(modalDialog(
        title="No Selection",
        "Please select a row in the table before clicking Download.",
        easyClose=TRUE,
        footer=modalButton("Close")
      ))
    }
  })

  observeEvent(input$ok, { # ok 버튼을 클릭하면 loaded_data를 업데이트하면서 modal 종료
    # loaded_data(loaded_data_val)
    loaded_data(input$upload$datapath)

    removeModal()
  })

  # download

  observeEvent(input$downloadButton, {
    if (!is.null(selected_wf_id())) {
      doc <- taskDB_init$find(paste0('{"id":', current_task_id_num(),"}"))
      structure_field <- doc$structure

      wf <- structure_field[[selected_wf_id()]]

      wfFiles <- wf[["wfFiles"]]

      #null값제외
      filenames <- sapply(wfFiles, function(x) if(!is.null(x$filename)) x$filename)

      filenames <- filenames[!sapply(filenames, is.null)]

      if (length(filenames) > 0) {
        showModal(modalDialog(
          title = h3("File Information"),
          selectInput("selected_files", label = h4("Select files to download"),
                      choices = setNames(filenames, filenames),
                      selected = NULL, multiple=TRUE),
          footer = tagList(
            footer=modalButton("Close"),
            downloadButton('downloadData', 'Download'),
          )
        ))

      } else {
        showModal(modalDialog(
          title="No Files",
          "파일이 없습니다.",
          easyClose=TRUE,
          footer=modalButton("Close")
        ))

        print("No files available.")

      }
    } else {
      showModal(modalDialog(
        title="No Selection",
        "Please select a row in the table before clicking Download.",
        easyClose=TRUE,
        footer=modalButton("Close")
      ))
    }
  })

  output$downloadData <- downloadHandler(
    # 파일 이름 설정
    filename = function() {
      # 여러개 파일 선택할 경우
      if (length(input$selected_files) > 1) {
        return("files.zip")
      } else {
        # 하나만 선택
        return(basename(input$selected_files[1]))
      }
    },

    # 파일 내용 설정
    content = function(file) {
      doc <- taskDB_init$find(paste0('{"id":', current_task_id_num(),"}"))
      structure_field <- doc$structure
      wf <- structure_field[[selected_wf_id()]]

      wfFiles <- wf[['wfFiles']]
      filenames <- sapply(wfFiles, function(x) if(!is.null(x$filename)) x$filename)

      if (length(input$selected_files) > 1) {
        temp_file_paths <- c()
        setwd(tempdir())

        # 선택한 파일들 하나씩 다운로드
        for (i in seq_len(length(input$selected_files))) {
          if (input$selected_files[i] %in% filenames) {
            #임시 파일 경로 생성
            temp_file_path <- file.path(getwd(), basename(input$selected_files[i]))
            # 다운로드
            taskFiles_init$download(input$selected_files[i], temp_file_path)
            # temp_file_paths에파일 이름 결합
            temp_file_paths <- c(temp_file_paths, basename(input$selected_files[i]))
          }
          print(paste(temp_file_paths))
        }

        # zip파일 압축
        zip_temp <- tempfile(fileext = ".zip")
        zip::zip(zipfile = zip_temp, files = temp_file_paths)
        file.copy(from = zip_temp, to = file)

      } else if (length(input$selected_files) == 1) {
        temp_file_path <- tempfile()
        taskFiles_init$download(input$selected_files[1], temp_file_path)
        file.copy(from = temp_file_path, to = file)

      } else {
        stop("No files were selected.")
      }

    }
  )

  # note save message
  observeEvent(input$note_save, {
    updateButton(session, 'note_save')
    showModal(modalDialog(
      title = "알림",
      "저장 되었습니다.",
      easyClose=TRUE,
      footer=modalButton("Close")
    ))
  })


  observeEvent(input$upnote_save, {
    updateButton(session,'upnote_save')
    showModal(modalDialog(
      title = "알림",
      "저장 되었습니다.",
      easyClose=TRUE,
      footer=modalButton("Close")
    ))
  })

  output$up_input <- renderUI({
    selected_up_id <- selected_up_id()
    selected_wf_id <- selected_wf_id()

    uiInput <- NULL

    if (!is.null(selected_wf_id) & length(selected_wf_id) > 0 & !is.null(selected_up_id) & length(selected_up_id) > 0) {
      for (upID in upDB_init$find()$upID) {
        if (is.character(upID)) {

          split_upID <- strsplit(upID, "_")[[1]]
          wfkey <- split_upID[1]
          upkey_num <- as.numeric(gsub("up", "", split_upID[2]))
          upkey <- paste0("up", upkey_num)

          if (wfkey == selected_wf_id & upkey == selected_up_id) {
            matched <- upDB_init$find(paste0('{"upID": "', upID, '"}'))

            if (is.list(matched) || is.data.frame(matched)) {
              if (is.list(matched$input) || is.data.frame(matched$input)) {
                upInput <- matched$input

                outputList <- lapply(seq_along(upInput), function(i) {
                  local({
                    my_i <- i

                    # 행열 바꿈
                    data <- data.frame(t(upInput[[my_i]]))
                    names(data) <- paste0(names(data), my_i)
                    DT::renderDataTable({
                      DT::datatable(data, options = list(searching = FALSE))
                    })
                  })
                })

                return(do.call(tagList, outputList))
              } else {
                print("The input of query result is not a list or data frame.")
              }
            } else {
              print("Query result is not a list or data frame.")
            }
          }
        }
      }
    }
  })


  #아이콘 그리기
  # button_state 넣어줄 reacitve value 생성
  # values <- reactiveValues()
  # values$states <- NULL
  #
  # # observe({
  # #   print(values$states)
  # # })
  #
  # # 첫 행 선택 시, tast_states()가 뜨게해야함.
  # observeEvent(input$task_DBtable_rows_selected, {
  #   values$states <- NULL
  # })
  #
  #
  # # task_states -> workflow, unitprocess key+state 한번에 반환
  # # -> task 행 선택 후 한번에 workflow, unit process 출력해야하기 때문
  # task_states <- reactive({
  #   req(input$task_DBtable_rows_selected)
  #
  #   #values$state의 값이 있는 경우, 그거 반환
  #   if (!is.null(values$states)) {
  #     return(values$states)
  #   }
  #
  #   selected_row <- input$task_DBtable_rows_selected[1]
  #   selected_task_id <- rv$df_tasklist[selected_row, "taskID"]
  #   selected_task <- taskDB_init$find(paste('{"taskID": "', selected_task_id, '"}', sep = ''))
  #   structure_field <- selected_task$structure
  #
  #   wf_up_states <- c()
  #
  #   for(wf_key in names(structure_field)) {
  #     wf <- structure_field[[wf_key]]
  #     wf_state <- wf[["wfState"]][[1]]
  #
  #     wf_up_states <- c(wf_up_states, paste(wf_key, wf_state, sep="_"))
  #
  #     up_keys <- names(wf)[grepl("^up", names(wf))]
  #
  #     for(up_key in up_keys) {
  #       up_state <- wf[[up_key]][["upState"]][[1]]
  #       wf_up_states <- c(wf_up_states, paste(up_key, up_state, sep="_"))
  #     }
  #   }
  #   return(wf_up_states)
  # })
  #
  #
  #
  # # observe({
  # #   print(task_states())
  # # })
  #
  # # button_state <- reactive({
  # #   req(input$button_state)
  # #   input$button_state
  # # })
  #
  #
  #
  # observeEvent(input$button_state,{
  #   new_states <- task_states()
  #   button_state_wf <- input$button_state[1]
  #
  #   button_state_up <- if(length(input$button_state) > 2) input$button_state[length(input$button_state)-1] else NULL
  #   state <- input$button_state[length(input$button_state)] #state는 workflow & unitprocess 둘 다 마지막 변수로 통일했음.
  #
  #   wf_key <- NULL
  #
  #   for(i in 1:length(new_states)){ #new_states->"wf1_1" "up1_2" "up2_1"
  #     split_state <- strsplit(new_states[i], "_")[[1]] #new_states를 분리함 key/state
  #     key <- split_state[1]
  #     if(grepl("^wf", key)){
  #       wf_key <- key #wf_key에 현재 workflowkey 저장함
  #       if(!is.null(button_state_wf) && key == button_state_wf && is.null(button_state_up)){
  #         #앞 조건 + button_state_up이 null일때만 wf 색 변경
  #         new_states[i] <- paste(key, state, sep = "_")
  #       }
  #     } else if(grepl("^up", key)) { #unitprocess 변경 할 경우
  #       if(!is.null(button_state_wf) && wf_key == button_state_wf && !is.null(button_state_up) && key == button_state_up){
  #         new_states[i] <- paste(key, state, sep = "_")
  #       }
  #     }
  #   }
  #   values$states <- new_states
  # })
  #
  #
  #
  # # task에 해당하는 workflow + unitprocess + icon 출력
  #
  #
  # output$up_plot <- renderPlot({
  #   task_states <- if (is.null(values$states)) task_states() else values$states
  #
  #   p <- ggplot() + ylim(0,3) + theme_void() + coord_fixed()
  #
  #   # 초기화
  #   wf_data <- data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0), color = character(0), alpha = numeric(0))
  #   up_data <- data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0), color = character(0), key = character(0))
  #   arrow_data <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))  # 화살표 데이터 프레임 초기화
  #
  #
  #   wf_index <- 0 # workflow 사각형 시작 시점 x좌표
  #   up_count <- 0 # wf에 속하는 unit 개수
  #
  #   for(i in 1:length(task_states)) {
  #     key <- strsplit(task_states[i], "_")[[1]][1]
  #     state <- as.character(strsplit(task_states[i], "_")[[1]][2])
  #
  #     color <- switch(state,
  #                     "2" =  "green",
  #                     "1" = "orange",
  #                     "0" =  "red",
  #                     "black")
  #
  #     gap_between_wf <- 2
  #     padding <- 0.2
  #
  #     if(startsWith(key, "wf")) {
  #       if (wf_index != 0) {
  #         wf_data$xmax[length(wf_data$xmax)] <- wf_index + padding + 1.2*up_count + padding
  #
  #         wf_data$color[length(wf_data$color)] <- color_wf
  #
  #         x_value <- 0.7+wf_index + padding + 1.2*up_count
  #         y_value <- 1.5
  #         xend_value <- 0.5+wf_index + padding + 1.2*up_count + padding + gap_between_wf / 2
  #         yend_value <- 1.5
  #
  #         if (!is.na(x_value) && !is.na(y_value) && !is.na(xend_value) && !is.na(yend_value)) {
  #           arrow_data <- rbind(arrow_data, data.frame(x = x_value, y = y_value, xend = xend_value, yend = yend_value))
  #         }
  #
  #         wf_index <- max(wf_data$xmax) + gap_between_wf
  #         up_count <- 0
  #       } else {
  #         wf_index <- wf_index + 8
  #       }
  #
  #       color_wf <- color  # wf의 색상을 업데이트
  #       wf_data <- rbind(wf_data, data.frame(xmin = wf_index, xmax = NA, ymin = 0.85, ymax = 2.15, color = NA, alpha = 0.5))
  #     }
  #
  #     if(startsWith(key, "up")) {
  #       up_count <- up_count + 1
  #       up_data <- rbind(up_data, data.frame(xmin = wf_index + padding + (up_count-1) * 1.2, xmax = wf_index + padding + up_count * 1.2, ymin = 1, ymax = 2, color = color, key = key))
  #     }
  #
  #     if(i == length(task_states)) {
  #       wf_data$xmax[length(wf_data$xmax)] <- wf_index + padding + 1.2*up_count + padding
  #       wf_data$color[length(wf_data$color)] <- color_wf
  #     }
  #   }
  #
  #   p <- p + geom_rect(data = wf_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color, alpha = alpha))
  #   p <- p + geom_rect(data = up_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color), color = "black", lwd = 0.2)
  #
  #   # suppressWarnings({
  #   #   p <- p + geom_segment(data = arrow_data, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed", angle = 30))
  #   # })
  #
  #   p <- p + geom_segment(data = arrow_data, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed", angle = 30))
  #
  #
  #   for(i in 1:length(task_states)) {
  #     key <- strsplit(task_states[i], "_")[[1]][1]
  #
  #     if (startsWith(key, "up")) {
  #       img <- paste0("C:/Users/0123456789/sblims-dev/KYJ/SBLIMS.progress/inst/img/", key, ".png")
  #       img <- system.file("img/", paste0(key, ".png"), package = "sblims2")
  #       if(file.exists(img)) {
  #         img_raster <- as.raster(readPNG(img))
  #         up_index <- which(up_data$key == key)
  #
  #         for(i in up_index) {
  #           center_up = (up_data$xmin[i] + up_data$xmax[i]) / 2
  #           p <- p + annotation_custom(rasterGrob(img_raster),
  #                                      xmin = center_up - 0.8,
  #                                      xmax = center_up + 0.8,
  #                                      ymin = 1,
  #                                      ymax = 2)
  #         }
  #       }
  #     }
  #   }
  #
  #   p <- p + scale_fill_identity(guide = 'none')
  #   p <- p + guides(alpha = 'none')
  #   p
  #
  # })

  #######################################################################
  # 마크다운

  textContent <- reactiveVal("")

  # 임시 폴더 경로 설정하고 이미지를 이 경로에만 저장 -> R세션 꺼지면 사라짐.
  temp_image_dir <- file.path(tempdir(), "images")
  if (!dir.exists(temp_image_dir)) {
    dir.create(temp_image_dir)
  }
  addResourcePath("images", temp_image_dir)

  observeEvent(input$file, {
    req(input$file)
    filePath <- input$file$datapath
    mdContent <- readLines(filePath, warn = FALSE)
    textContent(mdContent)

    updateAceEditor(session, "editor", value = paste(mdContent, collapse = "\n"))

    output$output <- renderUI({
      HTML(markdown::markdownToHTML(text = paste(textContent(), collapse = "\n"), fragment.only = TRUE))
    })
  })

  observe({
    input$editor
    isolate({
      if (!is.null(input$editor)) {
        textContent(strsplit(input$editor, "\n")[[1]])
      }
    })
  })

  output$saveBtn <- downloadHandler(
    filename = function() { paste0(Sys.Date(), "-modified.md") },
    content = function(file) {
      writeLines(textContent(), file)
    }
  )

  updateHeight <- function(elementId, height) {
    shinyjs::runjs(sprintf("$('%s').css('max-height', '%spx'); $('%s').css('height', '%spx');",
                           elementId, height, elementId, height))
  }

  observe({
    updateHeight('#renderingDiv', input$height)
    updateHeight('#editorDiv', input$height)
  })


  observeEvent(input$clipboard_image, {
    if (!is.null(input$clipboard_image)) {
      # base64로 인코딩된 이미지 디코딩-> binary로 변환
      image_data <- base64enc::base64decode(input$clipboard_image)
      #파일명기본 : 현재 시간, 특수문자는 .로 replace
      safe_filename <- gsub("[^a-zA-Z0-9]", ".", paste0(Sys.time(), ".png"))

      # 임시 file_path디렉토리에 파일 경로를 설정해줌
      file_path <- file.path(temp_image_dir, safe_filename)

      # filepath에 image_data넣어줌
      writeBin(image_data, file_path)

      #이미지 url생성
      image_url <- file.path("images", safe_filename)

      # 커서 위치에 이미지 삽입 -> ui쪽
      session$sendCustomMessage(type = "insertImage", message = list(url = image_url))
    }
  })

}

#' Title
#'
#' @return
#' @export
#'
#' @examples
# Manage_progress <- function(){
#   shinyApp(ui = ui(), server = server)
# }

shinyApp(ui = ui, server = server)
