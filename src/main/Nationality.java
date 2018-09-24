package main;

public class Nationality {

  private int nationalityId;
  private String nationalityTxt;

  public Nationality(String nationalityTxt) {
    this.nationalityId = -1;
    this.nationalityTxt = nationalityTxt;
  }

  @Override
  public String toString() {
    return "Nationality{" +
        "nationalityId=" + nationalityId +
        ", nationalityTxt=" + nationalityTxt +
        '}';
  }

  public int getNationalityId() {
    return nationalityId;
  }

  public void setNationalityId(int nationalityId) {
    this.nationalityId = nationalityId;
  }

  public String getNationalityTxt() {
    return nationalityTxt;
  }

  public void setNationalityTxt(String nationalityTxt) {
    this.nationalityTxt = nationalityTxt;
  }
}
