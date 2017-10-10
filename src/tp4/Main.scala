package tp4

object Main {
  def main(args : Array[String]) : Unit = {
    val exp=BinExpression("+",VariableRef("y"),BinExpression("-",IntegerValue(1),IntegerValue(2)))
    val prog=Seq(Assignement("x",IntegerValue(0)),
    					   Seq(Assignement("y",IntegerValue(1)),
              	     Seq(Read("z"),  
            					   Seq(While(BinExpression("<",VariableRef("x"),VariableRef("z")),
    					                     Seq(Assignement("x",BinExpression("+",VariableRef("x"),IntegerValue(1))),
    					    		  	             Seq(Assignement("y",BinExpression("*",VariableRef("y"),VariableRef("x"))),
    					    		  	                 Print(VariableRef("x"))))),
                    					Print(VariableRef("y"))))))
    		
                  
//    println("{\n"+PrettyPrinter.stringOf(prog)+"\n}");
    println(Interpret.eval(prog, List(5)));
  }
}

object PrettyPrinter {
  def stringOf(s: Statement) : String = {
    s match {
      case Seq(s1, s2) => stringOf(s1).concat(stringOf(s2));
      case If(c, s1, s2) => "if("+stringOf(c)+") {\n"+stringOf(s1)+"\n} else {\n"+stringOf(s2)+"\n}\n";
      case While(c, s) => "while("+stringOf(c)+")\n{\n"+stringOf(s)+"\n}\n";
      case Assignement(v, e) => v +" := "+stringOf(e)+"\n";
      case Print(e) => "print("+stringOf(e)+")";
      case Read(s) => "read("+s+")\n";
    }
  }
  
  def stringOf(e: Expression) : String = {
    e match {
      case IntegerValue(i) => i.toString();
      case VariableRef(s) => s;
      case BinExpression(op, e1, e2) => stringOf(e1)+" "+op+" "+stringOf(e2);
    }
  }
}

object Interpret {
  var m = Map[String, Int]();
  var ind = 0;
  def eval(p:Statement, inList:List[Int]) : List[Int] = {
    p match {
      case Seq(s1, s2) => eval(s1, inList) ++ eval(s2, inList);
      case If(c, s1, s2) => if(eval(c) != 0) eval(s1, inList) else eval(s2, inList);
      case While(c, s) => if(eval(c) != 0) eval(Seq(s, While(c, s)), inList) else List();
               
      case Assignement(v, e) => m += (v -> eval(e)); List();
      case Print(e) => List(eval(e));
      case Read(s) => m += (s -> inList(ind)) ; ind = ind +1 ; List();
    }
  }
  
  def eval(e : Expression) : Int = {
    e match {
      
      
      case IntegerValue(i) => i;
      
      
      case BinExpression("+", e1, e2) => eval(e1) + eval(e2);
      case BinExpression("-", e1, e2) => eval(e1) - eval(e2);
      case BinExpression("*", e1, e2) => eval(e1) * eval(e2);
      case BinExpression("/", e1, e2) => eval(e1) / eval(e2);
      case BinExpression("%", e1, e2) => eval(e1) % eval(e2);
      
      case BinExpression("<", e1, e2) => if(eval(e1) < eval(e2)) 1 else 0;
      case BinExpression(">", e1, e2) => if(eval(e1) > eval(e2)) 1 else 0;
      case BinExpression("<=", e1, e2) => if(eval(e1) <= eval(e2)) 1 else 0;
      case BinExpression(">=", e1, e2) => if(eval(e1) >= eval(e2)) 1 else 0;
      case BinExpression("==", e1, e2) => if(eval(e1) == eval(e2)) 1 else 0;
      case BinExpression("!=", e1, e2) => if(eval(e1) != eval(e2)) 1 else 0;
      
      case BinExpression("&&", e1, e2) => eval(e1) * eval(e2);
      case BinExpression("||", e1, e2) => eval(e1) + eval(e2);
      
      case BinExpression(_,e1,e2) => 0;
      
      
      case VariableRef(s) => m.getOrElse(s, 0);
    }
  }
  
}