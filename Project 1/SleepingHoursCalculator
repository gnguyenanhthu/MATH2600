import java.util.*;

public class Main
{
	public static void main(String[] args) {
	    String name;
	    final double cyclePeriod = 90.0;
	    int age, numberOfCycle, enterSleep, totalHour, totalMinute;
	    int startHour, startMinute, wakeHour, wakeMinute;
	    double totalSleep;
	    boolean alcohol;
	    Scanner input = new Scanner(System.in);
	    
	    System.out.println("Sleeping Hours Calculator");
		System.out.println("What's your name? ");
		name =  input.nextLine();
		System.out.println("How old are you? (please type negative number of months if a baby is younger than 1 year-old)");
		age = input.nextInt();
		System.out.println("What time do you go to bed? (9:30pm -> 21 30) ");
		startHour = input.nextInt(); startMinute = input.nextInt();
		System.out.println("Estimate how long does it take you enter sleep? (in minutes) ");
		enterSleep = input.nextInt();
		System.out.println("Do you drink alcohol? (True/False) ");
		alcohol = input.nextBoolean();
		
	    if (age >= -11){
	        if (age <= -4)
	            numberOfCycle = 9;
	        else if (age <= 0)
	            numberOfCycle = -11;
	        else if (age <= 5)
	            numberOfCycle = 8;
	        else if (age <= 13)
	            numberOfCycle = 7;
	        else if (age <= 25)
	            numberOfCycle = 6;
	        else
	            numberOfCycle = 5;
	    } else numberOfCycle = 5;
		    
		if (alcohol)
		    totalSleep = cyclePeriod * numberOfCycle + enterSleep + 15;
		else
		    totalSleep = cyclePeriod * numberOfCycle + enterSleep;
		
		totalHour = (int) Math.floor(totalSleep/60);
		totalMinute = (int) totalSleep - totalHour*60;
		
		wakeHour = startHour + totalHour;
		wakeMinute = startMinute + totalMinute;
		if (wakeMinute > 60){
		    wakeMinute -= 60;
		    ++wakeHour;
		}
		if (wakeHour > 24)
		    wakeHour -= 24;
		    
		System.out.println("------------Sleeping Hours Calculator------------");
		System.out.println("| Name: " + name);
		System.out.println("| Age: " + age);
		System.out.println("| Alcohol: " + alcohol);
		System.out.println("| Bed time: " + startHour + ":" + startMinute);
		System.out.println("| Wake up time: " + wakeHour + ":" + wakeMinute);
		System.out.println("| Total sleeping hours: " + totalHour + " hours " + totalMinute + " minutes");
		System.out.println("-------------------------------------------------");
		if(alcohol)
		    System.out.println("!!! It is recommended that you should NOT drink 4-6 hours prior to your sleep.");
		System.out.println("**Thank you for using calculator**");
		
	}
}
