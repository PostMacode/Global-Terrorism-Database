package main;

public class Country {

  private int countryId;
  private String countryName;


  public Country(String countryName) {
    this.countryId = -1;
    this.countryName = countryName;
  }

  @Override
  public String toString() {
    return "Country{" +
        "countryId=" + countryId +
        ", countryName=" + countryName +
        '}';
  }

  public int getCountryId() {
    return countryId;
  }

  public void setCountryId(int countryId) {
    this.countryId = countryId;
  }

  public String getCountryName() {
    return countryName;
  }

  public void setCountryName(String countryName) {
    this.countryName = countryName;
  }

}