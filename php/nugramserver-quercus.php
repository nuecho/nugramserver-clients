<?php
/*
 * NuGram Hosted Server client API in PHP.
 *
 * Copyright (C) 2009 Nu Echo Inc.
 *
 * This implementation relies heavily on the Quercus PHP
 * implementation, which runs on top of the Java Virtual Machine.
 *
 * See at the end of this file for a test example. The most important functions
 * are documented below.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software.
 *
 * The Software shall be used for Good, not Evil.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

import java.lang.StringBuffer;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;


// An object of this class acts as a proxy to NuGram Hosted Server.
class GrammarServer {
  public $host = null;

  function GrammarServer($host = "http://www.grammarserver.com:8082") {
    $this->host = $host;
  }

  function createSession($username, $password) {
    return new GrammarServerSession($this, $username, $password);
  }
}


// This class represents a session with NuGram Hosted Server.

class GrammarServerSession {
  public $server =  null;
  public $sessionid = null;
  public $username = null;
  public $password = null;
  public $authtoken = null;

  function GrammarServerSession($server, $username, $password) {
    $this->server = $server;
    $this->username = $username;
    $this->password = $password;
    $this->authtoken = base64_encode($username . ":" . $password);


    $response = __http_request($this->server->host . "/session",
			       'POST',
			       $this->authtoken,
			       array('responseFormat' => 'json'),
			       false);
    $this->sessionid = json_decode($response)->session->id;
  }

  // This method uploads a source grammar to NuGram Hosted Server.
  function upload($grammarpath, $content) {
    __http_request($this->server->host . "/grammar/" . $grammarpath,
		   'PUT',
		   $this->authtoken,
		   false,
		   $content);
    return true;
  }

  // This method requests NuGram Hosted Server to load a static grammar.
  function load($grammarpath)  {
    return $this->instantiate($grammarpath, array());
  }

  // This method instantiates a dynamic grammar and loads it.
  // The 'context' argument is expected to be a hash (dictionary) that
  // maps strings to values. Each value must be convertible to standard JSON.
  // Each key in the context must correspond to the name of a variable in the
  // ABNF template.
  function instantiate($grammarpath, $context) {
    $jsonContext = json_encode($context);
    $response = __http_request($this->server->host . "/grammar/" . $this->sessionid . "/" . $grammarpath,
			       'POST',
			       $this->authtoken,
			       array('responseFormat' => 'json', 'context' => $jsonContext),
			       false);
    return new InstantiatedGrammar($this, json_decode($response));
  }
  

  // Terminates the session with NuGram Hosted Server
  function disconnect() {
    __http_request($this->server->host . "/session/" . $this->sessionid,
		   'DELETE',
		   $this->authtoken,
		   array(),
		   false);
    return null;
  }
}

// Objects of this class act as proxy for instantiated grammars on NuGram Hosted Server.

class InstantiatedGrammar {
  public $session = null;
  public $data = null;

  function InstantiatedGrammar($session, $data) {
    $this->session = $session;
    $this->data = $data;
  }

  // Returns the URL of the grammar
  function getUrl($extension = 'abnf') {
    return $this->data->grammar->grammarUrl . "." . $extension;
  }
  
  // Retrieves the source representation of the grammar in the 
  // requested format ('abnf', 'grxml', or 'gsl')
  function getContent($extension = 'abnf') {
    return __http_request($this->getUrl($extension), 'GET', false, false, false);
  }

  // Computes the semantic interpretation of the given sentence
  // (which must a string). Returns a Python object of 'False' if
  // the sentence cannot be parsed by the grammar.
  function interpret($sentence) {
    $response = __http_request($this->data->grammar->interpreterUrl,
			       'POST',
			       $this->session->authtoken,
			       array('responseFormat' => 'json', 'sentence' => $sentence),
			       false);
    return json_decode($response)->interpretation;
  }

}

function __read_stream_as_string($inputStream) {
  $buffer = new StringBuffer();
  $reader = new BufferedReader(new InputStreamReader($inputStream));

  while ($reader->ready()) {
    $buffer->append($reader->readLine())->append("\n");
  }
  return ($buffer->toString());
}


function __http_request($url, $method, $authToken, $data, $text) {
  $connection = (new URL($url))->openConnection();
  $connection->setRequestMethod($method);
  if ($authToken) {
    $connection->setRequestProperty("Authorization", "Basic " . $authToken);
  }
  if ($data || $text) {
    $content = "";
    if ($text) {
      $content = $text;
    }
    else {
      foreach ($data as $param => $val) {
	$content =  $content . "&" . $param . "=" . $val;
      }
      $content = substr($content, 1);
    }
    if ($method != 'DELETE') {
      $connection->setDoOutput(true);
      $writer = new OutputStreamWriter($connection->getOutputStream());
      $writer->write($content);
      $writer->close();
    }
  }
  $connection->setConnectTimeout(5000);
  if ($connection->getResponseCode() >= 200 && $connection->getResponseCode() < 300) {
    $response = __read_stream_as_string($connection->getInputStream());
    $connection->disconnect();
    return $response;
  }
  else {
    $connection->disconnect();
    throw new Exception("com.nuecho.grammarserver.exception:" . $connection->getResponseMessage());
  }
}


function grammar_server_test() {

  $digitsGrammar = <<<EOF
#ABNF 1.0 ISO-8859-1;

language en-US;
tag-format <semantics/1.0>;

root \$digits;

public \$digits  = 
  @alt
    @for (digit : digits)
      @word digit
    @end
  @end
;
EOF;

  $server = new GrammarServer();
  $session = $server->createSession("USERNAME", "PASSWORD");
  $session->upload('digits.abnf', $digitsGrammar);
  print_r($session);
  echo "<br/><br/>";
  $grammar = $session->instantiate('digits.abnf', array('digits' => array("one", "two", "three")));
  print_r($grammar);
  echo "<br/><br/>";
  print_r($grammar->interpret('one'));
  echo "<br/><br/>";
  echo "<pre>" . $grammar->getContent() ". </pre>";
  $session->disconnect();
}

?>