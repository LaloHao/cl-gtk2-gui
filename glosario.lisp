(ql:quickload '(cl-cffi-gtk lambdalite))

(defpackage :glosario
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :common-lisp
        :lambdalite))

(in-package :glosario)

(defmacro within-top-level-window ((title &rest parameters) &body body)
  `(sb-int:with-float-traps-masked
       (:divide-by-zero)
     (within-main-loop
       (let ((window (make-instance 'gtk-window
                                  :title ,title
                                  :type :toplevel
                                  ,@parameters))
           (box (make-instance 'gtk-box
                               :orientation :vertical
                               :expand nil
                               :fill nil)))
         (g-signal-connect window "destroy"
                           (lambda (widget)
                             (declare (ignore widget))
                             (leave-gtk-main)))
         ,@body
         (gtk-container-add window box)
         (gtk-widget-show-all window)))))

(defmacro within-vbox ((&rest parameters) &rest rest)
  `(let ((pbox box)
       (box (make-instance 'gtk-box
                           :orientation :vertical
                           ,@parameters)))
     ,@rest
     (gtk-box-pack-start pbox box :expand nil)))

(defmacro within-hbox ((&rest parameters) &rest rest)
  `(let ((pbox box)
       (box (make-instance 'gtk-box
                           :orientation :horizontal
                           ,@parameters)))
     ,@rest
     (gtk-box-pack-start pbox box :expand nil)))

(defmacro label (text &rest parameters)
  `(let ((label (gtk-label-new ,text)))
     (gtk-box-pack-start box label ,@parameters)
     label))

(defmacro checkbox (&rest parameters)
  `(let ((button (gtk-check-button-new)))
     (gtk-box-pack-start box button ,@parameters)
     button))

(defmacro entry (name &rest parameters)
  `(let ((entry (let ((entry (make-instance 'gtk-entry)))
                (gtk-box-pack-start box entry ,@parameters)
                entry)))
     (g-signal-connect entry "changed"
                         (lambda (widget)
                           (declare (ignore widget))
                           (setf ,name (gtk-entry-text entry))))))

(defmacro textbox (name &rest parameters)
  `(let ((textbox (let ((textbox (make-instance 'gtk-text-view)))
                   (gtk-box-pack-start box textbox ,@parameters)
                   (gtk-text-view-buffer textbox))))
     (g-signal-connect textbox "changed"
                         (lambda (widget)
                           (declare (ignore widget))
                           (setf ,name (gtk-text-buffer-text textbox))))))

(defmacro button ((label &rest parameters) &body func)
  `(let ((button (gtk-button-new-with-label ,label)))
     (gtk-box-pack-start box button ,@parameters)
     (g-signal-connect button "clicked"
                       (lambda (widget)
                         (declare (ignore widget))
                         ,@func))))

(load-db :path "/tmp/db")

(defvar *anho-de-publicacion*)
(defvar *contexto*)
(defvar *fuente*)
(defvar *termino*)

(defun agregar-termino ()
  "Agrega el termino con todos sus campos a la base de datos cargada."
  (insert :terminos
          `(:/termino ,*termino*
                      :/fuente ,*fuente*
                      :/anho-de-publicacion ,*anho-de-publicacion*
                      :/contexto ,*contexto*)))

(defun ventana-de-agregar ()
  (within-top-level-window ("Agregar termino")
    (within-hbox (:spacing 0)
      (within-vbox (:spacing 0)
        (label "Término")
        (label "Fúente")
        (label "Año de publicacion")
        (label "Contexto"))
      (within-vbox (:spacing 0)
        (entry *termino*)
        (entry *fuente*)
        (entry *anho-de-publicacion*)
        (textbox *contexto*))
      (button ("Agregar") (agregar-termino)))))

(ventana-de-agregar)
