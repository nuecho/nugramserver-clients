package com.nuecho.grammarserver.api;

@SuppressWarnings("serial")
public class GrammarServerException extends Exception 
{

	public GrammarServerException(String message) 
	{
		super(message);
	}

	public GrammarServerException(Throwable cause) 
	{
		super(cause);
	}

	public GrammarServerException(String message, Throwable cause) 
	{
		super(message, cause);
	}

}
