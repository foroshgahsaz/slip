;;--------------=={ Count.lsp - Enhanced Block Counter }==--------------;;
;;                                                                      ;;
;;  This program enables the user to record the quantities of a         ;;
;;  selection or all standard or dynamic blocks in the working drawing. ;;
;;  The results of the block count may be displayed at the AutoCAD      ;;
;;  command-line, written to a Text or CSV file, or displayed in an     ;;
;;  AutoCAD Table, where available.                                     ;;
;;                                                                      ;;
;;  Enhanced version includes product code, row number and dimensions   ;;
;;  for each block in the table output.                                 ;;
;;                                                                      ;;
;;  Modified: August 2025 - Fixed dimensions and quantity logic         ;;
;;  Modified: June 2026 - Table print/PDF (4x scale) + count fix:       ;;
;;    - count:table-scale 4.0 (title 192, header 160, data 128)       ;;
;;    - Layer init moved out of file load (fixes Quick Select on load)  ;;
;;    - ssget selection fixed; sizes applied after table text fill      ;;
;;----------------------------------------------------------------------;;
;;  Original Author: Lee Mac, Copyright © 2014  -  www.lee-mac.com      ;;
;;----------------------------------------------------------------------;;

(setq
    count:version "2-3"
    count:defaults
   '(
        (out "tab")
        (tg1 "1")
        (tg2 "0")
        (tg3 "1")
        (tg4 "1")  ;; Toggle for product code
        (tg5 "1")  ;; Toggle for row number
        (tg6 "1")  ;; Toggle for dimensions
        (ed1 "Block Information")
        (ed2 "Preview")
        (ed3 "نام بلاک")
        (ed4 "تعداد")
        (ed5 "کد محصول")
        (ed6 "Row")
        (ed7 "ابعاد")
        (srt "blk")
        (ord "asc")
    )
)

;;----------------------------------------------------------------------;;

(defun count:fixdir ( dir )
    (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir))
)

;;----------------------------------------------------------------------;;

(defun count:getsavepath ( / tmp )
    (cond
        (   (setq tmp (getvar 'roamablerootprefix))
            (strcat (count:fixdir tmp) "\\Support")
        )
        (   (setq tmp (findfile "acad.pat"))
            (count:fixdir (vl-filename-directory tmp))
        )
        (   (count:fixdir (vl-filename-directory (vl-filename-mktemp))))
    )
)

;;----------------------------------------------------------------------;;

(setq count:savepath (count:getsavepath) ;; Save path for DCL & Config files
      count:dclfname (strcat count:savepath "\\LMAC_count_V" count:version ".dcl")
      count:cfgfname (strcat count:savepath "\\LMAC_count_V" count:version ".cfg")
)

;;----------------------------------------------------------------------;;

(defun count:acdoc nil
    (vla-get-activedocument (vlax-get-acad-object))
)

;;----------------------------------------------------------------------;;

;; Table print scale (4x base sizes for PDF/print)
(setq count:table-scale 4.0)

;;----------------------------------------------------------------------;;

(defun count:init-table-layer ( / layerName tableObjects )
    (setq layerName "Table")
    (if
        (setq tableObjects
            (ssget "_X" (list (cons 0 "*") (cons 8 layerName)))
        )
        (progn
            (command "._ERASE" tableObjects "")
            (prompt "\nPrevious Table layer objects deleted.")
        )
    )
    (if (not (tblsearch "LAYER" layerName))
        (command "._-LAYER" "_NEW" layerName "_COLOR" "1" layerName "")
        (command "._-LAYER" "_COLOR" "1" layerName "")
    )
    (setvar "CLAYER" layerName)
)

;;----------------------------------------------------------------------;;

(defun count:create-text-style ( style-name font-name / doc styles text-style )
    (setq doc (vla-get-activedocument (vlax-get-acad-object)))
    (setq styles (vla-get-textstyles doc))
    (if (not (tblsearch "STYLE" style-name))
        (progn
            (setq text-style (vla-add styles style-name))
            (vla-put-fontfile text-style font-name)
            (vla-put-height text-style 0.0)
            (vla-put-width text-style 1.0)
            (vla-put-obliqueangle text-style 0.0)
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:apply-table-print-size ( tab / s )
    (setq s count:table-scale)
    (vl-catch-all-apply 'vla-SetColumnWidth (list tab 0 (* 160.0 s)))
    (vl-catch-all-apply 'vla-SetColumnWidth (list tab 1 (* 400.0 s)))
    (vl-catch-all-apply 'vla-SetColumnWidth (list tab 2 (* 960.0 s)))
    (vl-catch-all-apply 'vla-SetColumnWidth (list tab 3 (* 320.0 s)))
    (vl-catch-all-apply 'vla-SetColumnWidth (list tab 4 (* 160.0 s)))
    (vl-catch-all-apply 'vla-SetTextHeight (list tab acTitleRow (* 48.0 s)))
    (vl-catch-all-apply 'vla-SetTextHeight (list tab acHeaderRow (* 40.0 s)))
    (vl-catch-all-apply 'vla-SetTextHeight (list tab acDataRow (* 32.0 s)))
    (vl-catch-all-apply 'vla-put-RowHeight (list tab (* 72.0 s)))
    (vl-catch-all-apply 'vla-SetRowHeight (list tab 0 (* 84.0 s)))
    (vl-catch-all-apply 'vla-SetRowHeight (list tab 1 (* 76.0 s)))
    (vl-catch-all-apply 'vla-SetAlignment (list tab acTitleRow acMiddleCenter))
    (vl-catch-all-apply 'vla-SetAlignment (list tab acHeaderRow acMiddleCenter))
    (vl-catch-all-apply 'vla-SetAlignment (list tab acDataRow acMiddleCenter))
)

;;----------------------------------------------------------------------;;

(defun count:get-product-code (ent / ename code attr_lst tag att)
  (vl-load-com)
  (setq ename (vlax-ename->vla-object ent))
  (if (and (vlax-property-available-p ename 'HasAttributes)
           (vla-get-hasattributes ename)
           (setq attr_lst (vlax-invoke ename 'GetAttributes)))
    (foreach att attr_lst
      (setq tag (strcase (vl-string-trim " " (vla-get-tagstring att))))
      (if (member tag '("CODE" "PRODUCT_CODE" "ID" "PART" "PART_NO" "PARTNO" "ITEM"))
        (setq code (vla-get-textstring att))
      )
    )
  )
  (if code 
    code 
    "N/A"
  )
)

;;----------------------------------------------------------------------;;
 
(defun count:get-dimensions-and-quantity (ent / ename dimensions_value meter_flag attr_lst tag att dim1 dim2 temp_str result pos1 pos2)
  (vl-load-com)
  (setq ename (vlax-ename->vla-object ent))
  (setq dimensions_value "N/A")
  (setq meter_flag nil)
  
  ;; Get attributes
  (if (and (vlax-property-available-p ename 'HasAttributes)
           (vla-get-hasattributes ename)
           (setq attr_lst (vlax-invoke ename 'GetAttributes)))
    (foreach att attr_lst
      (setq tag (strcase (vl-string-trim " " (vla-get-tagstring att))))
      
      ;; Get dimensions
      (if (member tag '("DIMENSIONS"))
        (setq dimensions_value (vla-get-textstring att))
      )
      
      ;; Get s-meter flag
      (if (member tag '("S-METER"))
        (setq meter_flag (strcase (vl-string-trim " " (vla-get-textstring att))))
      )
    )
  )
  
  ;; Return list with dimensions and calculated quantity
  (cond
    ;; If s-meter is YES and dimensions are in correct format
    ((and (equal meter_flag "YES") 
          (setq pos1 (vl-string-position 42 dimensions_value))  ; Find first *
          (setq temp_str (substr dimensions_value (+ pos1 2)))
          (setq pos2 (vl-string-position 42 temp_str)))         ; Find second *
     (progn
       ;; Parse dimensions (e.g., "152*85*62")
       (setq dim1 (atof (substr dimensions_value 1 pos1)))
       (setq dim2 (atof (substr temp_str 1 pos2)))
       
       ;; Return: (dimensions_string calculated_quantity)
       (list dimensions_value (/ (* dim1 dim2) 1000.0))
     )
    )
    
    ;; Default: return dimensions and quantity 1
    (t
     (list dimensions_value 1.0)
    )
  )
)

;;----------------------------------------------------------------------;;

(defun c:count
    (
        /
        *error*
        all
        col
        des dir
        ed1 ed2 ed3 ed4 ed5 ed6 ed7
        fil fnm fun
        hgt
        idx ins
        lst
        ord out
        row row_num
        sel srt
        tab tg1 tg2 tg3 tg4 tg5 tg6 tmp
        xrf
    )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'vla-object (type tab))
                 (null (vlax-erased-p tab))
                 (= "AcDbTable" (vla-get-objectname tab))
                 (vlax-write-enabled-p tab)
                 (vlax-property-available-p tab 'regeneratetablesuppressed t)
            )
            (vla-put-regeneratetablesuppressed tab :vlax-false)
        )
        (if (and (= 'vla-object (type count:wshobject))
                 (not (vlax-object-released-p count:wshobject))
            )
            (progn
                (vlax-release-object count:wshobject)
                (setq count:wshobject nil)
            )
        )
        (count:endundo (count:acdoc))
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (findfile count:cfgfname))
        (count:writecfg count:cfgfname (mapcar 'cadr count:defaults))
    )
    (count:readcfg count:cfgfname (mapcar 'car count:defaults))
    (foreach sym count:defaults
        (if (not (boundp (car sym))) (apply 'set sym))
    )
    (if (and (= "tab" out) (not (vlax-method-applicable-p (vla-get-modelspace (count:acdoc)) 'addtable)))
        (setq out "txt")
    )

    (count:startundo (count:acdoc))
    (count:init-table-layer)

    (while (setq tmp (tblnext "block" (null tmp)))
        (if (= 4 (logand 4 (cdr (assoc 70 tmp))))
            (setq xrf (vl-list* "," (cdr (assoc 2 tmp)) xrf))
        )
    )
    (if xrf
        (setq fil  (list '(0 . "INSERT") '(-4 . "<NOT") (cons 2 (apply 'strcat (cdr xrf))) '(-4 . "NOT>")))
        (setq fil '((0 . "INSERT")))
    )

    (cond
        (   (null (setq all (ssget "_X" fil)))
            (count:popup
                "No Blocks Found" 64
                (princ "No blocks were found in the active drawing.")
            )
        )
        (   (and (= "tab" out) (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer)))))))
            (count:popup
                "Current Layer Locked" 64
                (princ "Please unlock the current layer before using this program.")
            )
        )
        (   (progn
                (setvar 'nomutt 1)
                (princ "\nSelect blocks to count <all>: ")
                (setq sel (ssget "_:S" fil))
                (setvar 'nomutt 0)
                (if (not sel) (setq sel all))
                nil
            )
        )
        (   (or (= "com" out)
                (and (=  "tab" out) (setq ins (getpoint "\nSpecify point for table: ")))
                (and (/= "tab" out)
                    (setq fnm
                        (getfiled "Create Output File"
                            (cond
                                (   (and (setq dir (getenv "LMac\\countdir"))
                                         (vl-file-directory-p (setq dir (count:fixdir dir)))
                                    )
                                    (strcat dir "\\")
                                )
                                (   (getvar 'dwgprefix))
                            )
                            out 1
                        )
                    )
                )
            )

            ;; Process blocks
            (setq lst '())
            (repeat (setq idx (sslength sel))
                (setq ent (ssname sel (setq idx (1- idx))))
                (setq blk_name (count:effectivename ent))
                (setq prod_code (count:get-product-code ent))
                (setq dim_qty_result (count:get-dimensions-and-quantity ent))
                (setq dimensions (car dim_qty_result))
                (setq qty (cadr dim_qty_result))

                (setq blk_data (list blk_name prod_code "" dimensions))
                (setq lst (count:assoc-extended blk_data lst qty))
            )

            (princ "\n🔄 Sorting by product code...")

            (setq lst 
                (vl-sort lst 
                    '(lambda (a b) 
                        (< (atoi (cadar a)) (atoi (cadar b)))
                    )
                )
            )

            (princ "\n✅ Sorting completed!")

            ;; Add row numbers after sorting
                 (setq row_num 1)
                (setq new_lst '())
                (foreach item lst
                    (setq blk_data (car item))
                    (setq new_blk_data (list (nth 0 blk_data) (nth 1 blk_data) (itoa row_num) (nth 3 blk_data)))
                    (setq new_lst (append new_lst (list (cons new_blk_data (cdr item)))))
                    (setq row_num (1+ row_num))
                )
                (setq lst new_lst)

            (cond
                (   (= "com" out)
                    (defun prinn ( x ) (princ "\n") (princ x))
                    (prinn (count:padbetween "" "" "=" 80))
                    (if (= "1" tg1)
                        (progn
                            (prinn ed1)
                            (prinn (count:padbetween "" "" "-" 80))
                        )
                    )
                    ;; Create header
                    (setq header "")

                    (if (= "1" tg3) (setq header (strcat header ed4 "\t")))     ;; Quantity
                    (if (= "1" tg6) (setq header (strcat header ed7 "\t")))     ;; Dimensions
                    (if (= "1" tg3) (setq header (strcat header ed3 "\t")))     ;; Block Name
                    (if (= "1" tg4) (setq header (strcat header ed5 "\t")))     ;; Product Code
                    (if (= "1" tg5) (setq header (strcat header ed6)))          ;; Row

                    (prinn header)
                    (prinn (count:padbetween "" "" "-" 80))

                    ;; Print each row
                    (foreach itm lst
                        (setq row_data "")

                        (if (= "1" tg3) (setq row_data (strcat row_data (count:format-quantity (cdr itm)) "\t")))  ;; Quantity with decimals
                        
                        (if (= "1" tg6) (setq row_data (strcat row_data (cadddr (car itm)) "\t")))    ;; Dimensions
                        (if (= "1" tg3) (setq row_data (strcat row_data (caar itm) "\t")))           ;; Block Name
                        (if (= "1" tg4) (setq row_data (strcat row_data (cadar itm) "\t")))          ;; Product Code
                        (if (= "1" tg5) (setq row_data (strcat row_data (caddar itm))))              ;; Row

                        (prinn row_data)
                    )
                    (prinn (count:padbetween "" "" "=" 80))
                    (textpage)
                )
                (   (= "tab" out)
                    ;; Calculate column count
                    (setq col_count 0)
                    (if (= "1" tg3) (setq col_count (1+ col_count)))  ;; Quantity
                    (if (= "1" tg6) (setq col_count (1+ col_count)))  ;; Dimensions
                    (if (= "1" tg3) (setq col_count (1+ col_count)))  ;; Block Name
                    (if (= "1" tg4) (setq col_count (1+ col_count)))  ;; Product Code
                    (if (= "1" tg5) (setq col_count (1+ col_count)))  ;; Row

                    (setq hgt
                        (vla-gettextheight
                            (vla-item
                                (vla-item (vla-get-dictionaries (count:acdoc)) "acad_tablestyle")
                                (getvar 'ctablestyle)
                            )
                            acdatarow
                        )
                    )











                    (vl-load-com)
                    (count:create-text-style "B_Titr_Style" "B Yekan+")

                    (setq tab
                        (vla-addtable
                            (vlax-get-property (count:acdoc) (if (= 1 (getvar 'cvport)) 'paperspace 'modelspace))
                            (vlax-3D-point (trans ins 1 0))
                            (+ (length lst) 2)
                            col_count
                            (* 72.0 count:table-scale)
                            (* 180.0 count:table-scale)
                        )
                    )








                    (vla-put-stylename tab (getvar 'ctablestyle))
                    (vla-settextstyle tab acTitleRow "B_Titr_Style")
                    (vla-settextstyle tab acHeaderRow "B_Titr_Style")
                    (vla-settextstyle tab acDataRow "B_Titr_Style")

                    (if (vlax-property-available-p tab 'regeneratetablesuppressed t)
                        (vla-put-regeneratetablesuppressed tab :vlax-true)
                    )

                    ;; Create header row
                    (setq col 0)

                    (if (= "1" tg3) (progn (vla-settext tab 1 col ed4) (setq col (1+ col))))  ;; Quantity
                    (if (= "1" tg6) (progn (vla-settext tab 1 col ed7) (setq col (1+ col))))  ;; Dimensions
                    (if (= "1" tg3) (progn (vla-settext tab 1 col ed3) (setq col (1+ col))))  ;; Block Name
                    (if (= "1" tg4) (progn (vla-settext tab 1 col ed5) (setq col (1+ col))))  ;; Product Code  
                    (if (= "1" tg5) (progn (vla-settext tab 1 col ed6) (setq col (1+ col))))  ;; Row

                    ;; Fill table with data
                    (setq row 2)

                    (foreach row_item lst
                        (setq col 0)

                        (if (= "1" tg3) (progn 
                            (vla-settext tab row col (count:format-quantity (cdr row_item)))
                            (setq col (1+ col))))  ;; Quantity
                        (if (= "1" tg6) (progn 
                            (vla-settext tab row col (cadddr (car row_item))) 
                            (setq col (1+ col))))  ;; Dimensions
                        (if (= "1" tg3) (progn 
                            (vla-settext tab row col (caar row_item)) 
                            (setq col (1+ col))))  ;; Block Name
                        (if (= "1" tg4) (progn 
                            (vla-settext tab row col (cadar row_item)) 
                            (setq col (1+ col))))  ;; Product Code
                        (if (= "1" tg5) (progn 
                            (vla-settext tab row col (caddar row_item)) 
                            (setq col (1+ col))))  ;; Row

                        (setq row (1+ row))
                    )


                    

                    ;; Add title if enabled
                    (if (= "1" tg1)
                        (vla-settext tab 0 0 ed1)
                        (vla-deleterows tab 0 1)
                    )

                    ;; Apply 4x print sizes after table text is filled
                    (count:apply-table-print-size tab)

                    (if (vlax-property-available-p tab 'regeneratetablesuppressed t)
                        (vla-put-regeneratetablesuppressed tab :vlax-false)
                    )
                )
                (   (setenv "LMac\\countdir" (count:fixdir (vl-filename-directory fnm)))
                    ;; Prepare file data
                    (setq file_data '())

                    ;; Add title if enabled
                    (if (= "1" tg1)
                        (setq file_data (cons (list ed1) file_data))
                    )

                    ;; Create header row
                    (setq header '())
                    (if (= "1" tg3) (setq header (cons ed4 header)))  ;; Quantity
                    (if (= "1" tg6) (setq header (cons ed7 header)))  ;; Dimensions
                    (if (= "1" tg3) (setq header (cons ed3 header)))  ;; Block Name
                    (if (= "1" tg4) (setq header (cons ed5 header)))  ;; Product Code
                    (if (= "1" tg5) (setq header (cons ed6 header)))  ;; Row
                    (setq file_data (cons (reverse header) file_data))

                    ;; Add data rows
                    (foreach itm lst
                        (setq row_data '())
                        (if (= "1" tg3) (setq row_data (cons (count:format-quantity (cdr itm)) row_data)))  ;; Quantity
                        (if (= "1" tg6) (setq row_data (cons (cadddr (car itm)) row_data)))    ;; Dimensions
                        (if (= "1" tg3) (setq row_data (cons (caar itm) row_data)))           ;; Block Name
                        (if (= "1" tg4) (setq row_data (cons (cadar itm) row_data)))          ;; Product Code
                        (if (= "1" tg5) (setq row_data (cons (caddar itm) row_data)))         ;; Row
                        (setq file_data (cons (reverse row_data) file_data))
                    )
                    (setq file_data (reverse file_data))

                    ;; Write to file
                    (if
                        (
                            (if (= "txt" out)
                                count:writetxt
                                count:writecsv
                            )
                            file_data
                            fnm
                        )
                        (princ (strcat "\nBlock data written to " fnm))
                        (count:popup "Unable to Create Output File" 48
                            (princ
                                (strcat
                                    "The program was unable to create the following file:\n\n"
                                    fnm
                                    "\n\nPlease ensure that you have write-permissions for the above directory."
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (*error* nil)
    (princ)
)

(defun count:format-quantity ( qty )
    (if (= qty (fix qty))
        (itoa (fix qty))           ; برای اعداد صحیح: "1" به جای "1.00"
        (rtos qty 2 2)             ; برای اعداد اعشاری: "35.40"
    )
)

;;----------------------------------------------------------------------;;

(defun count:assoc-extended ( key-list lst qty / itm found )
    (setq found nil)
    (foreach item lst
        (if (and
                (equal (car (car item)) (car key-list))      ; Block name
                (or
                    (equal (cadr (car item)) (cadr key-list))    ; Product code
                    (and
                        (= (cadr (car item)) "N/A")
                        (= (cadr key-list) "N/A")
                    )
                )
                (or
                    (equal (cadddr (car item)) (cadddr key-list)) ; Dimensions
                    (and
                        (= (cadddr (car item)) "N/A")
                        (= (cadddr key-list) "N/A")
                    )
                )
            )
            (setq found item)
        )
    )
    (if found
        (subst (cons (car found) (+ (cdr found) qty)) found lst)  ;; Add to existing quantity
        (cons (cons key-list qty) lst)  ;; New entry with initial quantity
    )
)

;;----------------------------------------------------------------------;;

(defun c:countsettings
    (
        /
        *error*
        dch des
        ord out out-fun
        srt
        tg1 tg1-fun tg2 tg2-fun tg3 tg3-fun tg4 tg4-fun tg5 tg5-fun tg6 tg6-fun
    )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (and (= 'int (type dch))
                 (< 0 dch)
            )
            (unload_dialog dch)
        )
        (if (and (= 'vla-object (type count:wshobject))
                 (not (vlax-object-released-p count:wshobject))
            )
            (progn
                (vlax-release-object count:wshobject)
                (setq count:wshobject nil)
            )
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if (not (findfile count:cfgfname))
        (count:writecfg count:cfgfname (mapcar 'cadr count:defaults))
    )
    (count:readcfg count:cfgfname (mapcar 'car count:defaults))
    (foreach sym count:defaults
        (if (not (boundp (car sym))) (apply 'set sym))
    )
    (cond
        (   (not (count:writedcl count:dclfname))
            (count:popup "DCL file could not be written" 48
                (princ
                    (strcat
                        "The DCL file required by this program could not be written to the following location:\n\n"
                        count:dclfname
                        "\n\nPlease ensure that you have write-permissions for the above directory."
                    )
                )
            )
        )
        (   (<= (setq dch (load_dialog count:dclfname)) 0)
            (count:popup "DCL file could not be loaded" 48
                (princ
                    (strcat
                        "The following DCL file required by this program could not be loaded:\n\n"
                        count:dclfname
                        "\n\nPlease verify the integrity of this file."
                    )
                )
            )
        )
        (   (not (new_dialog "dia" dch))
            (count:popup "DCL file contains an error" 48
                (princ
                    (strcat
                        "The program dialog could not be displayed as the following DCL file file contains an error:\n\n"
                        count:dclfname
                        "\n\nPlease verify the integrity of this file."
                    )
                )
            )
        )
        (   t
            (set_tile "dcl"
                (strcat
                    "Count.lsp Version "
                    (vl-string-translate "-" "." count:version)
                    " \\U+00A9 Original by Lee Mac, Enhanced 2025"
                )
            )
            (if (and (= "tab" out) (not (vlax-method-applicable-p (vla-get-modelspace (count:acdoc)) 'addtable)))
                (progn
                    (mode_tile "tab" 1)
                    (setq out "txt")
                )
            )
            (   (setq tg1-fun (lambda ( val ) (mode_tile "ed1" (- 1 (atoi (setq tg1 val)))))) (set_tile "tg1" tg1))
            (action_tile "tg1" "(tg1-fun $value)")

            (   (setq tg2-fun (lambda ( val ) (mode_tile "ed2" (- 1 (atoi (setq tg2 val)))))) (set_tile "tg2" tg2))
            (action_tile "tg2" "(tg2-fun $value)")

            (   (setq tg3-fun (lambda ( val ) (mode_tile "ed3" (- 1 (atoi (setq tg3 val)))))) (set_tile "tg3" tg3))
            (action_tile "tg3" "(tg3-fun $value)")

            (foreach key '("ed1" "ed2" "ed3" "ed4" "ed5" "ed6" "ed7")
                (set_tile key (eval (read key)))
                (action_tile key (strcat "(setq " key " $value)"))
            )
            (set_tile out "1")
            (   (setq out-fun
                    (lambda ( val )
                        (if (= "tab" (setq out val))
                            (progn
                                (mode_tile "tg2" 0)
                                (mode_tile "ed2" (- 1 (atoi tg2)))
                            )
                            (progn
                                (mode_tile "tg2" 1)
                                (mode_tile "ed2" 1)
                            )
                        )
                    )
                )
                out
            )
            (foreach key '("tab" "txt" "csv" "com")
                (action_tile key "(out-fun $key)")
            )
            (set_tile srt "1")
            (foreach key '("blk" "qty")
                (action_tile key "(setq srt $key)")
            )
            (set_tile ord "1")
            (foreach key '("asc" "des")
                (action_tile key "(setq ord $key)")
            )
            (if (= 1 (start_dialog))
                (count:writecfg count:cfgfname (mapcar 'eval (mapcar 'car count:defaults)))
            )
        )
    )
    (*error* nil)
    (princ)
)

;;----------------------------------------------------------------------;;

(defun count:popup ( ttl flg msg / err )
    (setq err (vl-catch-all-apply 'vlax-invoke-method (list (count:wsh) 'popup msg 0 ttl flg)))
    (if (null (vl-catch-all-error-p err))
        err
    )
)

;;----------------------------------------------------------------------;;

(defun count:wsh nil
    (cond (count:wshobject) ((setq count:wshobject (vlax-create-object "wscript.shell"))))
)

;;----------------------------------------------------------------------;;

(defun count:tostring ( arg / dim )
    (cond
        (   (= 'int (type arg))
            (itoa arg)
        )
        (   (= 'real (type arg))
            (setq dim (getvar 'dimzin))
            (setvar 'dimzin 8)
            (setq arg (rtos arg 2 15))
            (setvar 'dimzin dim)
            arg
        )
        (   (vl-prin1-to-string arg))
    )
)

;;----------------------------------------------------------------------;;

(defun count:writecfg ( cfg lst / des )
    (if (setq des (open cfg "w"))
        (progn
            (foreach itm lst (write-line (count:tostring itm) des))
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:readcfg ( cfg lst / des itm )
    (if
        (and
            (setq cfg (findfile cfg))
            (setq des (open cfg "r"))
        )
        (progn
            (foreach sym lst
                (if (setq itm (read-line des))
                    (set sym (read itm))
                )
            )
            (setq des (close des))
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:writetxt ( lst txt / des )
    (if (setq des (open txt "w"))
        (progn
            (foreach itm lst (write-line (count:lst->str itm "\t") des))
            (close des)
            t
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:lst->str ( lst del )
    (if (cdr lst)
        (strcat (car lst) del (count:lst->str (cdr lst) del))
        (car lst)
    )
)

;;----------------------------------------------------------------------;;

(defun count:padbetween ( s1 s2 ch ln )
    (
        (lambda ( a b c )
            (repeat (- ln (length b) (length c)) (setq c (cons a c)))
            (vl-list->string (append b c))
        )
        (ascii ch)
        (vl-string->list s1)
        (vl-string->list s2)
    )
)

;;----------------------------------------------------------------------;;

(defun count:setblocktablerecord ( obj row col blk )
    (eval
        (list 'defun 'count:setblocktablerecord '( obj row col blk )
            (cons
                (if (vlax-method-applicable-p obj 'setblocktablerecordid32)
                    'vla-setblocktablerecordid32
                    'vla-setblocktablerecordid
                )
                (list
                    'obj 'row 'col
                    (list 'count:objectid (list 'vla-item (vla-get-blocks (count:acdoc)) 'blk))
                    ':vlax-true
                )
            )
        )
    )
    (count:setblocktablerecord obj row col blk)
)

;;----------------------------------------------------------------------;;

(defun count:objectid ( obj )
    (eval
        (list 'defun 'count:objectid '( obj )
            (cond
                (   (not (wcmatch (getenv "PROCESSOR_ARCHITECTURE") "*64*"))
                   '(vla-get-objectid obj)
                )
                (   (= 'subr (type vla-get-objectid32))
                   '(vla-get-objectid32 obj)
                )
                (   (list 'vla-getobjectidstring (vla-get-utility (count:acdoc)) 'obj ':vlax-false))
            )
        )
    )
    (count:objectid obj)
)

;;----------------------------------------------------------------------;;

(defun count:effectivename ( ent / blk rep )
    (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
        (if
            (and
                (setq rep
                    (cdadr
                        (assoc -3
                            (entget
                                (cdr
                                    (assoc 330
                                        (entget
                                            (tblobjname "block" blk)
                                        )
                                    )
                                )
                               '("AcDbBlockRepBTag")
                            )
                        )
                    )
                )
                (setq rep (handent (cdr (assoc 1005 rep))))
            )
            (setq blk (cdr (assoc 2 (entget rep))))
        )
    )
    blk
)

;;----------------------------------------------------------------------;;

(defun count:startundo ( doc )
    (count:endundo doc)
    (vla-startundomark doc)
)

;;----------------------------------------------------------------------;;

(defun count:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;;----------------------------------------------------------------------;;

(defun count:writedcl ( dcl / des )
    (cond
        (   (findfile dcl))
        (   (setq des (open dcl "w"))
            (foreach itm
               '(
                    "//--------------------=={ Count Dialog Definition }==-------------------//"
                    "//                                                                      //"
                    "//  Dialog definition file for use in conjunction with Count.lsp        //"
                    "//----------------------------------------------------------------------//"
                    "//  Original Author: Lee Mac, Copyright © 2014  -  www.lee-mac.com      //"
                    "//  Modified: April 2025 - Added product code, row number, dimensions   //"
                    "//----------------------------------------------------------------------//"
                    ""
                    "b15 : edit_box"
                    "{"
                    "    edit_width = 16;"
                    "    edit_limit = 1024;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "    horizontal_margin = none;"
                    "    vertical_margin = none;"
                    "}"
                    "b30 : edit_box"
                    "{"
                    "    edit_width = 52;"
                    "    edit_limit = 1024;"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "    horizontal_margin = none;"
                    "    vertical_margin = none;"
                    "}"
                    "tog : toggle"
                    "{"
                    "    vertical_margin = none;"
                    "    horizontal_margin = 0.2;"
                    "}"
                    "rwo : row"
                    "{"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    "rrw : radio_row"
                    "{"
                    "    fixed_width = true;"
                    "    alignment = centered;"
                    "}"
                    "dia : dialog"
                    "{"
                    "    key = \"dcl\";"
                    "    spacer_1;"
                    "    : boxed_column"
                    "    {"
                    "        label = \"Output\";"
                    "        : rrw"
                    "        {"
                    "            : radio_button { key = \"tab\"; label = \"Table\"; }"
                    "            : radio_button { key = \"txt\"; label = \"Text File\"; }"
                    "            : radio_button { key = \"csv\"; label = \"CSV File\"; }"
                    "            : radio_button { key = \"com\"; label = \"Command line\"; }"
                    "        }"
                    "        spacer;"
                    "    }"
                    "    : boxed_column"
                    "    {"
                    "        label = \"Headings\";"
                    "        spacer_1;"
                    "        : rwo"
                    "        {"
                    "            : tog { key = \"tg1\"; }"
                    "            : b30 { key = \"ed1\"; }"
                    "            : spacer"
                    "            {"
                    "                fixed_width = true;"
                    "                vertical_margin = none;"
                    "                width = 2.5;"
                    "            }"
                    "        }"
                    "        : rwo"
                    "        {"
                    "            spacer;"
                    "            : tog { key = \"tg2\"; }"
                    "            : b15 { key = \"ed2\"; }"
                    "            : b15 { key = \"ed3\"; }"
                    "            : b15 { key = \"ed4\"; }"
                    "            : tog { key = \"tg3\"; }"
                    "            spacer;"
                    "        }"
                    "        : rwo"
                    "        {"
                    "            spacer;"
                    "            : tog { key = \"tg4\"; }"
                    "            : b15 { key = \"ed5\"; }"
                    "            : b15 { key = \"ed6\"; }"
                    "            : b15 { key = \"ed7\"; }"
                    "            : tog { key = \"tg6\"; }"
                    "            spacer;"
                    "        }"
                    "        spacer_1;"
                    "    }"
                    "    : row"
                    "    {"
                    "        : boxed_column"
                    "        {"
                    "            label = \"Sort By\";"
                    "            : rrw"
                    "            {"
                    "                : radio_button { key = \"blk\"; label = \"Block Name\"; }"
                    "                : radio_button { key = \"qty\"; label = \"Quantity\"; }"
                    "            }"
                    "            spacer;"
                    "        }"
                    "        : boxed_column"
                    "        {"
                    "            label = \"Sort Order\";"
                    "            : rrw"
                    "            {"
                    "                : radio_button { key = \"asc\"; label = \"Ascending\"; }"
                    "                : radio_button { key = \"des\"; label = \"Descending\"; }"
                    "            }"
                    "            spacer;"
                    "        }"
                    "    }"
                    "    spacer_1; ok_cancel;"
                    "}"
                    ""
                    "//----------------------------------------------------------------------//"
                    "//                             End of File                              //"
                    "//----------------------------------------------------------------------//"
                )
                (write-line itm des)
            )
            (setq des (close des))
            (while (not (findfile dcl))) ;; for slow HDDs
            dcl
        )
    )
)

;;----------------------------------------------------------------------;;

(defun count:writecsv (lst csv / des sep)
  (if (setq des (open csv "w"))
      (progn
        ;; Get system list separator
        (setq sep (cond 
                   ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) 
                   (",")))
        
        ;; Write each row
        (foreach row lst 
          (write-line (str-join row sep) des))
        
        ;; Close file
        (close des)
        t
      )
    nil
  )
)

;;----------------------------------------------------------------------;;

(defun str-join (lst separator / result)
  (setq result "")
  (foreach item lst
    (setq result 
          (strcat result 
                 (if (= result "") "" separator)
                 (vl-princ-to-string item)
          )
    )
  )
  result
)

;;----------------------------------------------------------------------;;

(defun count:lst->csv ( lst sep )
    (if (cdr lst)
        (strcat (count:csv-addquotes (car lst) sep) sep (count:lst->csv (cdr lst) sep))
        (count:csv-addquotes (car lst) sep)
    )
)

;;----------------------------------------------------------------------;;

(defun count:csv-addquotes ( str sep / pos )
    (cond
        (   (wcmatch str (strcat "*[`" sep "\"]*"))
            (setq pos 0)
            (while (setq pos (vl-string-position 34 str pos))
                (setq str (vl-string-subst "\"\"" "\"" str pos)
                      pos (+ pos 2)
                )
            )
            (strcat "\"" str "\"")
        )
        (   str   )
    )
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: Count.lsp | Version "
        (vl-string-translate "-" "." count:version)
        " | Enhanced Block Counter ::"
        "\n:: \"count\" - Main Program | \"countsettings\" - Settings ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;">