package main;

public class Region {

  private int regionId;
  private String regionName;

  public Region(String regionName) {
    this.regionId = -1;
    this.regionName = regionName;
  }

  @Override
  public String toString() {
    return "Region{" +
        "regionId=" + regionId +
        ", regionName=" + regionName +
        '}';
  }

  public int getRegionId() {
    return regionId;
  }

  public void setRegionId(int regionId) {
    this.regionId = regionId;
  }

  public String getRegionName() {
    return regionName;
  }

  public void setRegionName(String regionName) {
    this.regionName = regionName;
  }
}
