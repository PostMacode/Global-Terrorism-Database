package main;

public class City {

  private int cityId;
  private String cityName;


  public City(String cityName) {
    this.cityId = -1;
    this.cityName = cityName;
  }

  @Override
  public String toString() {
    return "Country{" +
        "cityId=" + cityId +
        ", cityName=" + cityName +
        '}';
  }

  public int getCityId() {
    return cityId;
  }

  public void setCityId(int cityId) {
    this.cityId = cityId;
  }

  public String getCityName() {
    return cityName;
  }

  public void setCityName(String cityName) {
    this.cityName = cityName;
  }

}
