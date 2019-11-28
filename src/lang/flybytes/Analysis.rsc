module lang::flybytes::Analysis

import lang::flybytes::Syntax;
import IO;
import lang::flybytes::Decompiler;

public str file = (readFile(|home:///student.rsc|));

public Class bank = decompile(|project://MyBank/bin/business/Bank.class|);
public Class count = decompile(|project://MyBank/bin/business/Count.class|);
public Class student = decompile(|project://MyBank/bin/business/Student.class|);

//public Class cTest = decompile(|jar+project://MyBank/bin/business/appTest.jar!/business/Bank.class|);
//exists(|jar+project://MyBank/bin/business/appTest.jar!|);


public loc lc = |jar:///home/franciso/eclipse-workspace/MyBank/bin/business/appTest.jar!|;
 
public list[str] findClass(loc l){
	list[str] listClass = [];
	if (l.extension == "class")
		listClass = listClass + l.path;
	else{
		list[loc] pathAux = (l).ls;
		for (loc localization <-pathAux){
			listClass = listClass + findClass(localization);
		}	
	}
	return listClass;
}
public list[str] testList = findClass(lc);

