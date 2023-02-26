(require 'url)
(require 'json)

; change to your api token
(defconst APItoken "YOUR_API_KEY")
; you can change that if you want to use another ai
(defconst AImodel "text-davinci-003")
; you can change that if you want to use more or less tokens per request
(defconst MAXtokens 10)
; ypu can change that if you want to use other temperature
(defconst Temperature 0)

(defun aiQuery (input)
  "Send question to OpenAI API and get answer"
  (let* ((url-request-method "POST")
	   (url-request-extra-headers
	    `(("Content-Type" . "application/json")
	       ("Authorization" . ,(concat "Bearer " APItoken))))
	   (url-request-data
	    (json-encode `((model . ,AImodel)
			    (prompt . ,input)
			    (temperature . ,Temperature)
			    (max_tokens . ,MAXtokens))))
	   (url-http-response-status))
       (with-current-buffer (url-retrieve-synchronously "https://api.openai.com/v1/completions")
	 (goto-char url-http-end-of-headers)
	 (prog1
	     (json-read)
	   (kill-buffer)))))

(defun ask-ai (query)
  "Ask OpenAI question and get answer"
  (interactive "sAsk AI: ")
  (let ((response (aiQuery query)))
    (message "AI response: %s" (cdr (assoc 'text (aref (cdr (assoc 'choices response)) 0))))))
