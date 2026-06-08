(defun c:TagBlocksFromTable ( / tbl ent row blkName rowCount blkSet blkEnt blkObj minPt maxPt centerX bottomY insertPt layerName tagNumber)
  (vl-load-com)

  ;; نام لایه جدید
  (setq layerName "tag_map")

  ;; حذف فقط متن‌های موجود در لایه tag_map (برای جلوگیری از حذف جدول)
  (setq tagMapTexts (ssget "X" (list (cons 0 "TEXT") (cons 8 layerName))))
  (if tagMapTexts
    (progn
      (command "._ERASE" tagMapTexts "")
      (prompt (strcat "\n🗑️ Previous tags deleted from '" layerName "' layer."))
    )
  )

  ;; ایجاد لایه جدید tag_map با رنگ زرد (اگر وجود نداشت)
  (if (not (tblsearch "LAYER" layerName))
    (command "._-LAYER" "_NEW" layerName "_COLOR" "5" tag_map "")
    (command "._-LAYER" "_COLOR" "5" tag_map "") ;; اگر وجود داشت، فقط رنگ را آپدیت کن
  )
 


  ;; تنظیم لایه فعلی به tag_map
  (setvar "CLAYER" layerName)

  ;; انتخاب جدول (با پیام واضح)
  (while (not tbl)
    (setq tbl (car (entsel "\nSelect the table (avoid selecting other objects): ")))
    (if (not tbl)
      (prompt "\n❌ No table selected. Try again or press ESC to cancel.")
    )
  )

  (setq ent (vlax-ename->vla-object tbl))

  ;; تعداد سطرها
  (setq rowCount (vla-get-rows ent))

  ;; شروع شماره‌گذاری از 1
  (setq tagNumber 1)

  ;; پردازش از ردیف دوم (index=1)
  (setq row 1)
  (while (< row rowCount)
    ;; ستون سوم (index=2) = نام بلاک
    (setq blkName (vla-gettext ent row 2))
    (setq blkName (vl-string-trim " " blkName))

    (if (/= blkName "")
      (progn
        ;; پیدا کردن بلاک‌ها با این نام
        (setq blkSet (ssget "X" (list (cons 0 "INSERT") (cons 2 blkName))))
        (if blkSet
          (progn
            ;; درج شماره ردیف روی همه بلاک‌ها
            (repeat (sslength blkSet)
              (setq blkEnt (ssname blkSet 0))
              (ssdel blkEnt blkSet)

              ;; محاسبه نقطه درج متن (پایین مرکز بلاک)
              (setq blkObj (vlax-ename->vla-object blkEnt))
              (vla-getboundingbox blkObj 'minPt 'maxPt)
              (setq minPt (vlax-safearray->list minPt))
              (setq maxPt (vlax-safearray->list maxPt))
              (setq centerX (/ (+ (car minPt) (car maxPt)) 2.0))
              (setq bottomY (cadr minPt))
              (setq insertPt (list centerX (- bottomY 6.0) 0.0)) ; 3 واحد پایین‌تر

              ;; ایجاد متن در لایه tag_map
              (entmakex
                (list
                  (cons 0 "TEXT")
                  (cons 8 layerName)
                  (cons 10 insertPt)
                  (cons 11 insertPt)
                  (cons 1 (itoa tagNumber)) ; استفاده از tagNumber که از 1 شروع می‌شود
                  (cons 7 "Standard")
                  (cons 40 20) ; ارتفاع متن
                  (cons 72 1) ; تراز افقی: وسط
                  (cons 73 3) ; تراز عمودی: پایین
                )
              )
            )
            ;; افزایش شماره تگ فقط اگر بلاک پیدا شد
            (setq tagNumber (1+ tagNumber))
          )
        )
      )
    )
    (setq row (1+ row))
  )

  (prompt "\n✅ Tagging complete. Texts are in 'tag_map' layer.")
  (princ)
)