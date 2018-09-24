package main;

public class Outcome {

  private int eventId;
  private boolean suicide;
  private boolean multiple;
  private boolean success;
  private int numKilled;
  private int numInjured;

  public Outcome(boolean suicide,
      boolean multiple,
      boolean success,
      int numKilled,
      int numInjured) {
    this.eventId = -1;
    this.suicide = suicide;
    this.multiple = multiple;
    this.success = success;
    this.numKilled = numKilled;
    this.numInjured = numInjured;
  }

  @Override
  public String toString() {
    return "Outcome{" +
        "eventId=" + eventId +
        ", suicide=" + suicide +
        ", multiple=" + multiple +
        ", success=" + success +
        ", numKilled=" + numKilled +
        ", numInjured=" + numInjured +
        '}';
  }


  public int getEventId() {
    return eventId;
  }

  public void setEventId(int eventId) {
    this.eventId = eventId;
  }

  public boolean isSuicide() {
    return suicide;
  }

  public void setSuicide(boolean suicide) {
    this.suicide = suicide;
  }

  public boolean isMultiple() {
    return multiple;
  }

  public void setMultiple(boolean multiple) {
    this.multiple = multiple;
  }

  public boolean isSuccess() {
    return success;
  }

  public void setSuccess(boolean success) {
    this.success = success;
  }

  public int getNumKilled() {
    return numKilled;
  }

  public void setNumKilled(int numKilled) {
    this.numKilled = numKilled;
  }

  public int getNumInjured() {
    return numInjured;
  }

  public void setNumInjured(int numInjured) {
    this.numInjured = numInjured;
  }
}
