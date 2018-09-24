package main;

public class EventTarget {
  private int eventTargetId;
  private int eventId;
  private int targetId;

  public EventTarget(int eventId, int targetId) {
    this.eventTargetId = -1;
    this.eventId = eventId;
    this.targetId = targetId;
  }

  @Override
  public String toString() {
    return "EventTarget{" +
        "event_target_id=" + eventTargetId +
        ", event_id=" + eventId +
        ", target_id=" + targetId +
        "}";
  }

  public int getEventTargetID() {
    return eventTargetId;
  }

  public void setEventTargetID(int eventTargetId) {
    this.eventTargetId = eventTargetId;
  }

  public int getEventId() {
    return eventId;
  }

  public void setEventId(int eventId) {
    this.eventId = eventId;
  }

  public int getTargetId() {
    return targetId;
  }

  public void setTargetId(int targetId) {
    this.targetId = targetId;
  }
}
