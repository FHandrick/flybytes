module lang::flybytes::Service
import lang::flybytes::Analysys;
import lang::flybytes::Syntax;
import List;
import IO;

public list[Field] listFields(Class c) = c.fields;
public list[str] listFieldsName(list[Field] f) = [name | /str name <- f];

public str className(Class c){
	str clname = "Class";
	top-down visit (c) {
  		case class(object(cName)): clname = cName;
  	}
  	return clname;
}

public list[str] methodsName(list[Method] m){
	list[str] mRet = [];
	for (Method mth <-m){
		top-down visit (mth) {
  			case method(methodDesc(_,name,_),_,_): mRet=mRet+name; 
  		}
	}
	return mRet;
}
//methodsName(listMethods(count));

public list[Method] listMethods(Class c) = c.methods;
//listMethods(myClass);

public list[Signature] listMethodsDesc(list[Method] m) = [sig | /Signature sig <- m];
//listMethodsDesc(listMethods(myClass));

public list[Stat] listStatMethodsEsp(list[Method] m, str ms){
	Signature sig;
	Method mRet = static([]);
	for (Method mth <-m){
		sig = mth.desc;
		if (sig != constructorDesc([])){
			if (sig.name==ms)
				mRet = mth;	
		}
	}
	return mRet.block;
}
//listStatMethodsEsp(listMethods(bank),"auditor")

public Method methodsEsp(list[Method] m, str ms){
	Method mRet = static([]);
	for (Method mth <-m){ 
		top-down visit (mth) {
			case method(methodDesc(_,ms,_),_,_): mRet=mth;
  		}
	}
	return mRet;
}
//methodsEsp(listMethods(bank),"auditor")

public list[str] listMethodsRastName(Method m) {
 	list[str] lm = [];  
  	top-down visit (m) {
  		case invokeVirtual(_, _, methodDesc(_,n,_), _): lm=lm+n; 
  	}
  	return lm;
}
//listMethodsRastName(methodsEsp(listMethods(bank),"auditor"));

public list[str] listAllMethodName(Class c)
= methodsName(listMethods(c));
//listAllMethodName(myClass);

public list[list[str]] rastMethod(Class c,str mFather){
	list[list[str]] rastList = [[mFather]];
	list[str] lmChild = listMethodsRastName(methodsEsp(listMethods(c),mFather));
	for (str mChild <- lmChild)
		rastList = rastList+[[mFather,mChild]];
	return rastList;
}
//rastMethod(bank,"auditor")

public list[list[str]] rastAllMethod(Class c){
	list[list[str]] rastList = [];
	list[str] allMethod = listAllMethodName(c);
	for(str m <- allMethod)
		rastList = rastList+rastMethod(c,m);
	return rastList;
}
//rastAllMethod(bank)
public list[list[str]] rastAllMethodMtOne(Class c){
	return [s | s<-rastAllMethod(c), size(s)>=2];
}
//rastAllMethodMtOne(bank)

public set[tuple[str,str]] graph(Class c){
	set[tuple[str,str]] s = {};
	for(str methoL <- listAllMethodName(c)){
		s = s+{<className(c),methoL>};
	}
	for (list[str] l <-rastAllMethodMtOne(c))
		s = s+{<l[0],l[1]>};
	return s;
}
