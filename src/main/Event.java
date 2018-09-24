package main;

public class Event {

  private int eventId;
  private int year;
  private int month;
  private int day;
  private boolean doubtTerr;
  private String summary;
  private int numPerps;
  private int locationId;
  private int attacktypeId;
  private int targetTypeId;
  private int weaponTypeId;

  public Event(
      int year,
      int month,
      int day,
      boolean doubtTerr,
      String summary,
      int numPerps,
      int locationId,
      int attackTypeId,
      int targetTypeId,
      int weaponTypeId) {

    this.eventId = -1;
    this.year = year;
    this.month = month;
    this.day = day;
    this.doubtTerr = doubtTerr;
    this.summary = summary;
    this.numPerps = numPerps;
    this.locationId = locationId;
    this.attacktypeId = attackTypeId;
    this.targetTypeId = targetTypeId;
    this.weaponTypeId = weaponTypeId;
  }

  @Override
  public String toString() {
    return "Event{" +
        "eventId=" + eventId +
        ", year=" + year +
        ", month=" + month +
        ", day=" + day +
        ", doubtTerr=" + doubtTerr +
        ", summary=" + summary +
        ", numPerps=" + numPerps +
        ", locationId=" + locationId +
        ", attacktypeId=" + attacktypeId +
        ", targettypeId=" + targetTypeId +
        ", weapontypeId=" + weaponTypeId +
        '}';
  }

  public int getEventId() {
    return eventId;
  }

  public void setEventId(int eventId) {
    this.eventId = eventId;
  }

  public int getYear() {
    return year;
  }

  public void setYear(int year) {
    this.year = year;
  }

  public int getMonth() {
    return month;
  }

  public void setMonth(int month) {
    this.month = month;
  }

  public int getDay() {
    return day;
  }

  public void setDay(int day) {
    this.day = day;
  }

  public boolean isDoubtTerr() {
    return doubtTerr;
  }

  public void setDoubtTerr(boolean doubtTerr) {
    this.doubtTerr = doubtTerr;
  }

  public String getSummary() {
    return summary;
  }

  public void setSummary(String summary) {
    this.summary = summary;
  }

  public int getNumPerps() {
    return numPerps;
  }

  public void setNumPerps(int numPerps) {
    this.numPerps = numPerps;
  }

  public int getLocationId() {
    return locationId;
  }

  public void setLocationId(int locationId) {
    this.locationId = locationId;
  }

  public int getAttacktypeId() {
    return attacktypeId;
  }

  public void setAttacktypeId(int attacktypeId) {
    this.attacktypeId = attacktypeId;
  }

  public int getTargetTypeId() {
    return this.targetTypeId;
  }

  public void setTargetTypeId(int targetTypeId) {
    this.targetTypeId = targetTypeId;
  }

  public int getWeaponTypeId() {
    return this.weaponTypeId;
  }

  public void setWeaponTypeId(int weaponTypeId) {
    this.weaponTypeId = weaponTypeId;
  }
}


