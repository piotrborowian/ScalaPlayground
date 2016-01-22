package week3;

class A {} 
class B extends A{}

abstract class C {
  abstract A fun();
}

class D extends C {
	@Override A fun() {
		return new B();
	}
}

public class JavaPlayground {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		System.out.println((new D()).fun());

	}

}
