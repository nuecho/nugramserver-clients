;;; NuGram Hosted Server client API in Clojure
;;
;; Copyright (C) 2009 Nu Echo Inc.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:



(ns #^{
    :doc "NuGram Hosted Server client API in Clojure
 "
    :author "Nu Echo Inc."}

    nuecho.nugramserver.api
    (:require [clojure.contrib.http.agent :as h-a]
	      [clojure.contrib.base64 :as base64]
	      [clojure.contrib.str-utils :as str]
	      [clojure.contrib.json :as json]))



(def *default-grammar-extension* "abnf")
(def *supported-grammar-formats* #{"abnf" "grxml" "gsl"})
(def *nugram-hosted-server* "www.grammarserver.com")


(defn- first-arg-type [arg & rest]
  (:type arg))
(defn- check-type [arg type]
  (assert (= (:type arg) type)))


(defmulti server-url (fn [obj path] (type obj)))
(defmethod server-url java.util.Map [session path]
  (server-url (:host session) path))
(defmethod server-url String [host path]
  (.concat "https://" (.concat host path)))


(defn server-request
  ([session path method]
   (server-request session path method nil false))
  ([session path method body]
   (server-request session path method body false))
  ([session path method body absolute-path?]
   (h-a/http-agent (if absolute-path?
		     path
		     (server-url session path))
		   :headers {"Authorization" (:auth-str session)}
		   :method  method
		   :read-timeout 1000
		   :body body)))


(defn process-request [request-agent handler]
  (await request-agent)
  (cond (h-a/success? request-agent)
	(handler (h-a/string request-agent))
	true
	(throw (java.lang.RuntimeException. "Unable to connect with NuGram Server"))))

  
(defn create-session
  ([#^String username #^String password]
   (create-session *nugram-hosted-server* username password))
  ([#^String host #^String username #^String password]
   (let [auth-str (.concat "Basic " (base64/encode-str (.concat username (.concat ":" password))))]
     (process-request 
      (server-request {:host host :auth-str auth-str} "/api/session" "POST" "responseFormat=json")
      (fn [body]
	  (let [json-response (json/read-json body)
		session       (get json-response :session)
		session-id    (get session :id)]
	    {:type       :NuGramServerSession
	    :host        host
	    :session-id  session-id
	    :auth-str    auth-str}))))))


(defn disconnect [session]
  (check-type session :NuGramServerSession)
  (process-request
   (server-request session (.concat "/api/session/" (:session-id session)) "DELETE")
   (fn [body] true)))


(defn upload [session #^String path #^String content]
  (check-type session :NuGramServerSession)
  (process-request
   (server-request session (.concat "/api/grammar/" path) "PUT" content)
   (fn [body] true)))


(defn instantiate [session #^String path context]
  (check-type session :NuGramServerSession)
  (let [json-context (json/json-str context)]
    (process-request
     (server-request session
		     (str/str-join "/" ["" "api" "grammar" (:session-id session) path])
		     "POST"
		     (.concat "responseFormat=json&context=" json-context))
     (fn [body]
	 (let [json-response (json/read-json body)
	       grammar       (get json-response :grammar)
	       grammar-url   (get grammar :grammarUrl)
	       interp-url    (get grammar :interpreterUrl)]
           (println json-response)
           (println grammar)
           (println grammar-url)
	   {:type :InstantiatedGrammar
	   :session session
	   :grammar-url grammar-url
	   :interpreter-url interp-url})))))


(defn activate [session #^String path]
  (instantiate session path {}))


(defn grammar-url
  ([grammar]
   (grammar-url grammar *default-grammar-extension*))
  ([grammar extension]
   (check-type grammar :InstantiatedGrammar)
   (assert (contains? *supported-grammar-formats* extension))
   (str/str-join "." [(:grammar-url grammar) extension])))


(defn grammar-content
  ([grammar]
   (grammar-content grammar *default-grammar-extension*))
  ([grammar #^String extension]
   (check-type grammar :InstantiatedGrammar)
   (assert (contains? *supported-grammar-formats* extension))
   (let [url   (grammar-url grammar extension)
	 agent (h-a/http-agent url)]
     (await agent)
     (h-a/string agent))))


(defn interpret [grammar #^String sentence]
  (check-type grammar :InstantiatedGrammar)
  (process-request
   (server-request (:session grammar)
		   (:interpreter-url grammar)
		   "POST"
		   (.concat "responseFormat=json&sentence=" sentence)
		   true)
   (fn [body]
       (let [json-response  (json/read-json body)]
         (get json-response :interpretation)))))



(defn api-test [username password]
  (let [session (create-session username password)]
    (upload session "digits.abnf" "#ABNF 1.0 ISO-8859-1;\n\nlanguage en-US;\ntag-format <semantics/1.0>;\n\nroot $digits;\n\npublic $digits  = \n@alt\n    @for (digit : digits)\n        @word digit\n    @end\n@end\n;")
    (let [grammar (instantiate session "digits.abnf" {"digits" ["one" "two" "three"]})]
      (println (grammar-content grammar "abnf"))
      (println (interpret grammar "one")))
    (disconnect session)))

