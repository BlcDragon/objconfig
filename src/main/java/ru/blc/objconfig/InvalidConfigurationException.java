package ru.blc.objconfig;

public class InvalidConfigurationException extends Exception{

	private static final long serialVersionUID = -5997145576181088728L;

	public InvalidConfigurationException() {
		super();
	}

	public InvalidConfigurationException(String msg) {
		super(msg);
	}

	public InvalidConfigurationException(Throwable cause) {
		super(cause);
	}

	public InvalidConfigurationException(String msg, Throwable cause) {
		super(msg, cause);
	}
}
