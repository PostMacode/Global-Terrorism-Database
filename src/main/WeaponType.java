package main;

public class WeaponType {

  private int weaponTypeId;
  private String weaponTypeTxt;

  public WeaponType(String weaponTypeTxt) {
    this.weaponTypeId = -1;
    this.weaponTypeTxt = weaponTypeTxt;
  }

  @Override
  public String toString() {
    return "WeaponType{" +
        "weaponTypeId=" + weaponTypeId +
        ", weaponTypeTxt=" + weaponTypeTxt +
        "}";
  }

  public int getWeaponTypeId() {
    return weaponTypeId;
  }

  public void setWeaponTypeId(int weaponTypeId) {
    this.weaponTypeId = weaponTypeId;
  }

  public String getWeaponTypeTxt() {
    return weaponTypeTxt;
  }

  public void setWeaponTypeTxt(String weaponTypeTxt) {
    this.weaponTypeTxt = weaponTypeTxt;
  }
}