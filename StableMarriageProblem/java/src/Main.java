
public class Main {

	public static void main(String[] args) {
		Person m1 = new Person("1", Gender.MALE);
		Person m2 = new Person("2", Gender.MALE);
		Person m3 = new Person("3", Gender.MALE);
		Person m4 = new Person("4", Gender.MALE);
		Person a1 = new Person("1", Gender.FEMALE);
		Person a2 = new Person("2", Gender.FEMALE);
		Person a3 = new Person("3", Gender.FEMALE);
		Person a4 = new Person("4", Gender.FEMALE);
		
		m1.addToPreferenceList(a4);
		m1.addToPreferenceList(a1);
		m1.addToPreferenceList(a2);
		m1.addToPreferenceList(a3);
		m2.addToPreferenceList(a2);
		m2.addToPreferenceList(a3);
		m2.addToPreferenceList(a1);
		m2.addToPreferenceList(a4);
		m3.addToPreferenceList(a2);
		m3.addToPreferenceList(a4);
		m3.addToPreferenceList(a3);
		m3.addToPreferenceList(a1);
		m4.addToPreferenceList(a3);
		m4.addToPreferenceList(a1);
		m4.addToPreferenceList(a4);
		m4.addToPreferenceList(a2);
		
		a1.addToPreferenceList(m4);
		a1.addToPreferenceList(m1);
		a1.addToPreferenceList(m3);
		a1.addToPreferenceList(m2);
		a2.addToPreferenceList(m1);
		a2.addToPreferenceList(m3);
		a2.addToPreferenceList(m2);
		a2.addToPreferenceList(m4);
		a3.addToPreferenceList(m1);
		a3.addToPreferenceList(m2);
		a3.addToPreferenceList(m3);
		a3.addToPreferenceList(m4);
		a4.addToPreferenceList(m4);
		a4.addToPreferenceList(m1);
		a4.addToPreferenceList(m3);
		a4.addToPreferenceList(m2);
		
		Matching matching = new Matching();
		matching.addPerson(m1);
		matching.addPerson(m2);
		matching.addPerson(m3);
		matching.addPerson(m4);
		matching.addPerson(a1);
		matching.addPerson(a2);
		matching.addPerson(a3);
		matching.addPerson(a4);
		matching.doMatching();
	}

}
