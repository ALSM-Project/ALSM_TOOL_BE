package jp.co.alsm.common;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class SqlDto {
	private String id;
	private String content;
	private List<Object> parameters = new ArrayList<Object>();
}
