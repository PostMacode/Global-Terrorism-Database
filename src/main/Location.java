package main;

public class Location {

  private int locationId;
  private int countryId;
  private int regionId;
  private int provStateId;
  private int cityId;
  private double latitude;
  private double longitude;


  public Location(int countryId,
      int regionId,
      int provstateId,
      int cityId,
      double latitude,
      double longitude) {
    this.locationId = -1;
    this.countryId = countryId;
    this.regionId = regionId;
    this.provStateId = provstateId;
    this.cityId = cityId;
    this.latitude = latitude;
    this.longitude = longitude;
  }

  @Override
  public String toString() {
    return "Location{" +
        "locationId=" + locationId +
        ", countryId=" + countryId +
        ", regionId=" + regionId +
        ", provStateId=" + provStateId +
        ", cityId=" + cityId +
        ", latitude=" + latitude +
        ", longitude=" + longitude +
        '}';
  }

  public int getLocationId() {
    return locationId;
  }

  public void setLocationId(int locationId) {
    this.locationId = locationId;
  }

  public int getCountryId() {
    return countryId;
  }

  public void setCountryId(int countryId) {
    this.countryId = countryId;
  }

  public int getRegionId() {
    return regionId;
  }

  public void setRegionId(int regionId) {
    this.regionId = regionId;
  }

  public int getProvStateId() {
    return provStateId;
  }

  public void setProvStateId(int provStateId) {
    this.provStateId = provStateId;
  }

  public int getCityId() {
    return cityId;
  }

  public void setCityId(int city_id) {
    this.cityId = city_id;
  }

  public double getLatitude() {
    return latitude;
  }

  public void setLatitude(double latitude) {
    this.latitude = latitude;
  }

  public double getLongitude() {
    return longitude;
  }

  public void setLongitude(double longitude) {
    this.longitude = longitude;
  }
}
