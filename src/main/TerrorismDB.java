package main;

public class TerrorismDB {

  private static TerrorismAPI api = new TerrorismMySQL();

  public static void main(String[] args) throws Exception {

    // Initialize the connection
    api.setConnection("jdbc:mysql://localhost:3306/terrorism", "root", "smokeup");

    // Insert an event
    // Exception thrown if event already exists
    System.out.println("Registering a terrorism event...");
    Event e = new Event(2018, 8, 14, false, null, 1, 21, 7, 2, 10);
    int eid = api.insertEvent(e);
    System.out.println("Newly inserted Event:" + eid);

    // Test adding weapon types
    System.out.println("Adding weapon types...");
    int wid1 = api.getOrInsertWeaponType("Octopus");
    int wid2 = api.getOrInsertWeaponType("Giant Squid");
    int wid3 = api.getOrInsertWeaponType("Turtle Shell");
    int wid4 = api.getOrInsertWeaponType("Firearms");
    int widtest = api.getOrInsertAttackType("Octopus");
    System.out.println("Weapontype IDs: " + wid1 + " " + wid2 + " " + wid3 + " " + wid4 + " " + widtest);

    api.closeConnection();
  }
}