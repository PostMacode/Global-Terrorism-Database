package main;

public class AttackType {

  private int attacktypeId;
  private String attacktypeTxt;


  public AttackType(String attacktypeTxt) {
    this.attacktypeId = -1;
    this.attacktypeTxt = attacktypeTxt;
  }

  @Override
  public String toString() {
    return "Country{" +
        "attacktypeId=" + attacktypeId +
        ", attacktypeTxt=" + attacktypeTxt +
        '}';
  }

  public int getAttacktypeId() {
    return attacktypeId;
  }

  public void setAttacktypeId(int attacktypeId) {
    this.attacktypeId = attacktypeId;
  }

  public String getAttacktypeTxt() {
    return attacktypeTxt;
  }

  public void setAttacktypeTxt(String attacktypeTxt) {
    this.attacktypeTxt = attacktypeTxt;
  }

}

