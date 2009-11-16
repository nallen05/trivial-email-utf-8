
(defpackage :trivial-email-utf-8
  (:use :cl)
  (:export :send-email*
	   :invite-user))

(in-package :trivial-email-utf-8)

;; see http://www.openspf.org/Best_Practices/Webgenerated
          
(defun .qprint-encode/utf-8 (string)
  (qprint:encode (map 'string
                      'code-char
                      (trivial-utf-8:string-to-utf-8-bytes string))))

(defun .unicode-p (string)
  "returns utf-8 encoded bytes if string needs to be uft-8 encoded"
  (let ((bytes (trivial-utf-8:string-to-utf-8-bytes string)))
    (if (mismatch bytes (map 'vector 'char-code string))
        bytes)))

(defun .qprint-encode/utf-8-email-title (string)
  (cl-ppcre:regex-replace-all "\\S+"
                              string
                              (list (lambda (word)
                                      (if (not (.unicode-p word))
                                          word
                                          (format nil
                                                  "=?UTF-8?Q?~A?="
                                                  (.qprint-encode/utf-8 word)))))
                              :simple-calls t))

(defun send-email* (host from to subject message 
		    &rest kwd-args
		    &key (port 25) cc bcc reply-to extra-headers
		         display-name authentication
			 attachments (buffer-size 256))
  "like CL-SMTP:SEND-EMAIL but qprints subject and message [utf-8]
and sends headers appropriately when it encounters unicode"
  (declare (ignore port cc bcc reply-to display-name
		   authentication attachments buffer-size))
  (let ((unicodep (.unicode-p message))
        (cl-smtp::*content-type* "text/plain; charset=utf-8"))
    (apply 'cl-smtp:send-email
	   host
	   from
	   to
	   (.qprint-encode/utf-8-email-title subject)
	   (if unicodep
	       (.qprint-encode/utf-8 message)
	       message)
	   (if unicodep
               (list* :extra-headers `((:content-transfer-encoding "quoted-printable")
                                       ,@extra-headers)
                      kwd-args)
	       kwd-args))))

(defun invite-user (host server-email-address invited-by invited subject message 
		    &rest kwd-args
		    &key (port 25) cc bcc reply-to extra-headers
		         display-name authentication
			 attachments (buffer-size 256))
  "
like SEND-EMAIL* except it sends the email to `INVITED' from `SERVER-EMAIL-ADDRESS'
on behalf of the email address `INVITED-BY'
"
  (declare (ignore port cc bcc reply-to display-name
		   authentication attachments buffer-size))
  (apply 'send-email*
	 host
	 invited-by
	 invited
	 subject
	 message
	 (list* :extra-headers `((:reply-to ,invited-by)
				 (:sender ,server-email-address)
				 (:return-path ,server-email-address)
				 ,@extra-headers)
		kwd-args)))

;; (defun email/unicode (to from title content)
;;   (cl-smtp:send-email "localhost"
;;                       from
;;                       to
;;                       (.qprint-encode/utf-8-email-title title)
;;                       (.qprint-encode/utf-8 content)
;;                       :extra-headers  `((:content-transfer-encoding "quoted-printable")
;;                                         (:mime-version "1.0")
;;                                         (:content-type "text/plain; charset=utf-8")
;;                                         (:reply-to ,from)
;;                                         (:sender "noreply@example.com")
;;                                         (:return-path "noreply@example.com"))))

;; (defun send-invite (to from title content)
;;   (if (.unicode-p content)
;;       (email/unicode to from title content)
;;       (email/ascii to from title content)))