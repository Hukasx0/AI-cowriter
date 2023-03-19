(require 'url)
(require 'json)

; change to your api token
(defconst APItoken "YOUR_API_KEY")
; you can change that if you want to use another ai
(defconst AImodel "text-davinci-003")
; you can change that if you want to use more or less tokens per request
(defconst MAXtokens 50)
; you can change that if you want to use other temperature
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

(defun code-bugfix-ai ()
  "Send your selected code to AI and get corrected code"
  (interactive)
  (let ((codeResp (aiQuery (concat "Fix bugs in code below" "\n" (buffer-substring-no-properties (region-beginning) (region-end)) ) ) ))
  (kill-region (region-beginning) (region-end))
  (insert (cdr (assoc 'text (aref (cdr (assoc 'choices codeResp)) 0))))))

(defun code-complete-ai ()
  "Send your selected code/instructions to AI and get coding help"
  (interactive)
  (let ((codeResp (aiQuery (buffer-substring-no-properties (region-beginning) (region-end) ) ) ))
  (insert (cdr (assoc 'text (aref (cdr (assoc 'choices codeResp)) 0))))))

(defun explain-code-ai ()
  "Send your selected code to AI for clarification"
  (interactive)
  (let ((explanation (aiQuery (concat "Explain what this code does" "\n"  (buffer-substring-no-properties (region-beginning) (region-end)) ) )))
    (message "AI explanation: %s"  (cdr (assoc 'text (aref (cdr (assoc 'choices explanation)) 0))))))

(defun translate-ai (query)
  "Send selected text/code to AI for translation to another spoken/programming language"
  (interactive "sLanguage: ")
  (let ((translation (aiQuery (concat (concat "Translate text below to %s" " " query) "\n"  (buffer-substring-no-properties (region-beginning) (region-end)) ) )))
    (insert (cdr (assoc 'text (aref (cdr (assoc 'choices translation)) 0))))))

(defun tell-joke-ai (query)
  "Ask AI to generate a joke on a specific topic"
  (interactive "sJoke topic: ")
  (let ((response (aiQuery (concat "Tell me a joke about" " " query) )))
    (message "AI response: %s" (cdr (assoc 'text (aref (cdr (assoc 'choices response)) 0))))))

(defun explain-topic-ai (query)
  "Ask AI to explain something"
  (interactive "sTopic: ")
  (let ((response (aiQuery (concat "Explain" query " to me") )))
    (message "AI response: %s" (cdr (assoc 'text (aref (cdr (assoc 'choices response)) 0))))))

(defun coding-motivation-ai ()
  "Ask AI to motivate you to code"
  (interactive)
  (let ((response (aiQuery "motivate me to code")))
    (message "AI response: %s" (cdr (assoc 'text (aref (cdr (assoc 'choices response)) 0))))))

(defun ask-ai (query)
  "Ask OpenAI question and get answer"
  (interactive "sAsk AI: ")
  (let ((response (aiQuery query)))
    (message "AI response: %s" (cdr (assoc 'text (aref (cdr (assoc 'choices response)) 0))))))
