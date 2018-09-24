package main;

import database.DBUtils;

public class TerrorismMySQL implements TerrorismAPI {

  // need a constructor
  private DBUtils dbu;

  @Override
  public Event getEventById(int eventId) {
    return null;
  }

  @Override
  public int insertEvent(Event e) {
    String sql =
        "INSERT INTO event(event_id,year,month,day,doubterr,summary,num_perps,"
            + "location_id,attacktype_id,target_id,weapontype_id) VALUES "
            + "('" + e.getEventId() + "','" + e.getYear() + "','" + e.getMonth()
            + "','" + e.getDay() + "','" + e.isDoubtTerr() + "','" + e.getSummary()
            + "','" + e.getNumPerps() + "','" + e.getLocationId() + "','" + e.getAttacktypeId()
            + "','" + e.getTargetTypeId() + "','" + e.getWeaponTypeId() + ")";
    return dbu.insertOneRecord(sql);
  }

  @Override
  public int getOrInsertAttackType(String attackType) {
    return dbu.getOrInsertTerm("attacktype", "attacktype_id", "attacktype_txt", attackType);
  }

  @Override
  public int getOrInsertCity(String city) {
    return dbu.getOrInsertTerm("city", "city_id", "city_name", city);
  }

  @Override
  public int getOrInsertCountry(String country) {
    return dbu.getOrInsertTerm("country", "country_id", "country_name", country);
  }

  @Override
  public int insertEventTarget(EventTarget e) {
    String sql =
        "INSERT INTO eventtarget(event_id,target_id) VALUES" + "('" + e.getEventId() + "','"
            + e.getTargetId() + ")";
    return dbu.insertOneRecord(sql);
  }

  @Override
  public int insertLocation(Location l) {
    String sql =
        "INSERT INTO location(location_id,country_id,region_id,provstate_id,city_id,latitude,longitude) VALUES"
            + "('" + l.getLocationId() + "','" + l.getCountryId() + "','" + l.getRegionId() + "','"
            + l
            .getProvStateId() + "','" + l.getCityId() + "','" + l.getLatitude() + "','" + l
            .getLongitude() + ")";
    return dbu.insertOneRecord(sql);
  }

  @Override
  public int getOrInsertNationality(String nationality) {
    return dbu.getOrInsertTerm("nationality", "nationality_id", "nationality_txt", nationality);
  }

  @Override
  public int insertOutcome(Outcome o) {
    String sql =
        "INSERT INTO outcome(event_id,suicide,multiple,success,num_killed,num_injured) VALUES"
            + "('"
            + o.getEventId() + "','" + o.isSuicide() + "','" + o.isMultiple() + "','" +
            o.isSuccess() + "','" + o.getNumKilled() + "','" + o.getNumInjured() + ")";
    return dbu.insertOneRecord(sql);
  }

  @Override
  public int getOrInsertProvstate(String provstate) {
    return dbu.getOrInsertTerm("provstate", "provstate_id", "provstate_name", provstate);
  }

  @Override
  public int getOrInsertRegion(String region) {
    return dbu.getOrInsertTerm("region", "region_id", "region_name", region);
  }

  @Override
  public int getOrInsertTarget(String target) {
    return dbu.getOrInsertTerm("target", "target_id", "entity_name", target);
  }

  @Override
  public int getOrInsertTargetType(String targettype) {
    return dbu.getOrInsertTerm("targettype", "targettype_id", "targettype_txt", targettype);
  }

  @Override
  public int getOrInsertWeaponType(String weapontype) {
    return dbu.getOrInsertTerm("weapontype", "weapontype_id", "weapontype_txt", weapontype);
  }

  @Override
  public void setConnection(String url, String user, String password) {
    dbu = new DBUtils(url, user, password);
  }

  @Override
  public void closeConnection() {
    dbu.closeConnection();
  }
}