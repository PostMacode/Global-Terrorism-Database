package main;

public interface TerrorismAPI {

  /**
   * Retrieve event by its ID If no event with the given ID exists, return -1
   *
   * @param eventId the ID of the event
   * @return Event matching the given ID
   */
  Event getEventById(int eventId);

  /**
   * Insert the given Event.
   *
   * @param e the event
   * @return ID of newly created Event or -1 if already exists
   */
  int insertEvent(Event e);

  /**
   * Get or insert on attacktype term
   *
   * @param attackType the attacktype
   * @return ID of a new or existing attacktype
   */
  int getOrInsertAttackType(String attackType);

  /**
   * Get or insert on city term
   *
   * @param city the city
   * @return ID of a new or existing city
   */
  int getOrInsertCity(String city);

  /**
   * Get or insert on country term
   *
   * @param country the country
   * @return ID of a new or existing country
   */
  int getOrInsertCountry(String country);

  /**
   * Insert one EventTarget
   *
   * @param e the EventTarget
   * @return ID of newly created EventTarget or -1 if already exists
   */
  int insertEventTarget(EventTarget e);

  /**
   * Insert one Location
   *
   * @param l the location
   * @return ID of newly created Location or -1 if already exists
   */
  int insertLocation(Location l);

  /**
   * Get or insert on Nationality term
   *
   * @param nationality the nationality
   * @return ID of new or existing nationality
   */
  int getOrInsertNationality(String nationality);

  /**
   * Insert outcome
   *
   * @param o the outcome
   * @return ID of newly created nationality or -1 if already exists
   */
  int insertOutcome(Outcome o);

  /**
   * Get or insert on Provstate term
   *
   * @param provstate the Provstate
   * @return ID of new or existing Provstate
   */
  int getOrInsertProvstate(String provstate);

  /**
   * Get or insert on Region term
   *
   * @param region the Region
   * @return ID of new or existing Region
   */
  int getOrInsertRegion(String region);

  /**
   * Get or insert on Target term
   *
   * @param target the Target
   * @return ID of new or existing Target
   */
  int getOrInsertTarget(String target);

  /**
   * Get or insert on TargetType term
   *
   * @param targettype the TargetType
   * @return ID of new or existing TargetType
   */
  int getOrInsertTargetType(String targettype);

  /**
   * Get or insert on WeaponType term
   *
   * @param weapontype the WeaponType
   * @return ID of new or existing WeaponType
   */
  int getOrInsertWeaponType(String weapontype);

  /**
   * Set connection settings
   */
  void setConnection(String url, String user, String password);

  /**
   * Close the connection when application finishes
   */
  void closeConnection();
}