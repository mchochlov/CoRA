FROM openjdk:11-jre-slim
RUN apt-get update && apt-get install --no-install-recommends -y openjfx
COPY target/lib/*[^win]* /usr/src/cora/lib/
COPY target/cora-0.1.7-SNAPSHOT.jar /usr/src/cora
WORKDIR /usr/src/cora
ENTRYPOINT ["java", "--module-path", "lib", "--add-modules=javafx.controls,javafx.fxml", "--add-opens", "javafx.graphics/javafx.scene.text=ALL-UNNAMED", "--add-opens", "javafx.graphics/com.sun.javafx.text=ALL-UNNAMED", "--add-exports", "javafx.graphics/com.sun.javafx.geom=ALL-UNNAMED", "--add-exports", "javafx.graphics/com.sun.javafx.text=ALL-UNNAMED", "--add-exports", "javafx.graphics/com.sun.javafx.scene.text=ALL-UNNAMED", "-jar", "cora-0.1.7-SNAPSHOT.jar"]
