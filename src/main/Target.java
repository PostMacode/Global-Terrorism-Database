package main;

public class Target {

  private int targetId;
  private int targetTypeId;
  private String entityName;
  private String specificTargetName;
  private int nationalityId;

  public Target(int targettypeId,
      String entityName,
      String specificTargetName,
      int nationalityId) {
    this.targetId = -1;
    this.targetTypeId = targettypeId;
    this.entityName = entityName;
    this.specificTargetName = specificTargetName;
    this.nationalityId = nationalityId;
  }

  @Override
  public String toString() {
    return "Region{" +
        "targetId=" + targetId +
        ", targetTypeId=" + targetTypeId +
        ", entityName=" + entityName +
        ", specificTargetName=" + specificTargetName +
        ", nationalityId=" + nationalityId +
        '}';
  }

  public int getTargetId() {
    return targetId;
  }

  public void setTargetId(int targetId) {
    this.targetId = targetId;
  }

  public int getTargetTypeId() {
    return targetTypeId;
  }

  public void setTargetTypeId(int targetTypeId) {
    this.targetTypeId = targetTypeId;
  }

  public String getEntityName() {
    return entityName;
  }

  public void setEntityName(String entityName) {
    this.entityName = entityName;
  }

  public String getSpecificTargetName() {
    return specificTargetName;
  }

  public void setSpecificTargetName(String specificTargetName) {
    this.specificTargetName = specificTargetName;
  }

  public int getNationalityId() {
    return nationalityId;
  }

  public void setNationalityId(int nationalityId) {
    this.nationalityId = nationalityId;
  }
}
