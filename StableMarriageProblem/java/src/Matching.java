import java.util.LinkedList;
import java.util.List;

public class Matching {

	private List<Person> men;
	private List<Person> women;
	
	
	public Matching() {
		men = new LinkedList<>();
		women = new LinkedList<>();
	}
	
	public boolean addPerson(Person p) {
		if (p.getGender() == Gender.MALE)
			return this.men.add(p);
		return this.women.add(p);
	}
	
	public void doMatching() {
		this.setEveryoneFree(men);
		this.setEveryoneFree(women);
		
		while (this.thereIsAnyManFree()) {
			System.out.printf("\n>>>> THERE ARE %d MEN FREE\n", howManyManAreFree());
			for (Person man : men) {
				if (!man.isFree())
					continue;
				
				Person woman = null;
				
				try {
					woman = man.popNotYetProposedPerson();
				} catch (Exception e) {
					System.out.printf("\n>>> %s EMPTY STACK!", man.toString());
				}
				
				if (woman.isFree()) {
					man.setMatchingPartner(woman);
					woman.setMatchingPartner(man);
					System.out.printf("\n>>> (%s) MARRIED (%s)\n", woman.toString(), man.toString());
				} else if (woman.prefersSomeoneOverCurrentMatchingPartner(man)) {
					woman.getMatchingPartner().setFree();
					man.setMatchingPartner(woman);
					woman.setMatchingPartner(man);
					System.out.printf("\n>>> (%s) MARRIED (%s)\n", woman.toString(), man.toString());
				} else {
					System.out.printf("\n>>> (%s) REJECTED (%s)\n", woman.toString(), man.toString());
				}
			}
		}
		
		System.out.println("{");
		for (Person man : men) {
			System.out.printf("\t(%s; %s), \n", man.toString(), man.getMatchingPartner().toString());
		}
		System.out.println("}");
	}
	
	private void setEveryoneFree(List<Person> l) {
		for (Person p : l)
			if (!p.isFree())
				p.setFree();
	}
	
	private boolean thereIsAnyManFree() {
		for (Person man : men)
			if (man.isFree())
				return true;
		return false;
	}
	
	private int howManyManAreFree() {
		int count = 0;
		for (Person man : men) 
			if (man.isFree())
				count++;
		return count;
	}
}
