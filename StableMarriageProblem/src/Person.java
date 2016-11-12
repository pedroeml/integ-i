import java.security.InvalidParameterException;
import java.util.EmptyStackException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

public class Person {
	
	private String name;
	private Gender gender;
	private List<Person> preferenceList;
	private List<Person> notYetProposed;
	private Person matchingPartner;
	private boolean free;
	
	public Person(String name, Gender gender) {
		this.name = name;
		this.gender = gender;
		this.preferenceList = new LinkedList<>();
		this.notYetProposed = new LinkedList<>();
		this.matchingPartner = null;
		this.free = true;
	}

	public Person getMatchingPartner() {
		return matchingPartner;
	}
	
	/**
	 * 
	 */
	public void setMatchingPartner(Person matchingPartner) throws InvalidParameterException {
		if (matchingPartner == null)
			throw new InvalidParameterException("Argument is null");
		this.matchingPartner = matchingPartner;
		this.free = false;
	}

	public String getName() {
		return name;
	}
	
	public Gender getGender() {
		return gender;
	}
	
	public boolean isFree() {
		return this.free;
	}
	
	/**
	 * Removes current matching partner and set boolean attribute 'free' to true.
	 * 
	 */
	public void setFree() {
		this.matchingPartner = null;
		this.free = true;
	}
	
	/**
	 * Adds a new Person object to the prefence list and also to the not yet proposed stack.
	 * 
	 */
	public boolean addToPreferenceList(Person p) {
		this.notYetProposed.add(p);
		return this.preferenceList.add(p);
	}
	
	/**
	 * Pops a not yet proposed person from the stack.
	 * 
	 */
	public Person popNotYetProposedPerson() throws Exception {
		if (this.notYetProposed.isEmpty())
			throw new Exception("Not Yeat Proposed Stack is empty!");
		return this.notYetProposed.remove(0);
	}
	
	/**
	 * Compares if the index of Person p0 is before than Person p1 inside the prefenrece list.
	 * 
	 */
	private boolean prefersAoverB(Person p0, Person p1) throws NoSuchElementException {
		int i0 = this.preferenceList.indexOf(p0);	// Isso deve ser otimizado, pois são feitas duas iterações sobre a lista para 
		int i1 = this.preferenceList.indexOf(p1);	// descobrir o índice de dois elementos na lista. Isso pode ser feito em somente uma iteração!
		
		if (i0 == -1)
			throw new NoSuchElementException(String.format("The person %s doesn't exist in the preference list of %s.", p0.toString(), this.toString()));
		else if (i1 == -1)
			throw new NoSuchElementException(String.format("The person %s doesn't exist in the preference list of %s.", p1.toString(), this.toString()));
		
		return i0 < i1 ? true : false;
	}
	
	/**
	 * Compares if the index of Person p is before than the current matcbing partner.
	 * 
	 */
	public boolean prefersSomeoneOverCurrentMatchingPartner(Person p) throws NoSuchElementException {
		if (p == this.getMatchingPartner())
			return false;
		return this.prefersAoverB(p, this.getMatchingPartner());
	}
	
	@Override
	public String toString() {
		return String.format("%s, %s, %s", this.getName(), this.getGender(), this.isFree() ? "Single" : "Married");
	}
}
