FROM openjdk:11-jre-slim
RUN apt-get update && apt-get install --no-install-recommends -y openjfx dbus-x11 at-spi2-core
WORKDIR /usr/src/cora
COPY target .
ENTRYPOINT ["java", "--module-path", "lib", "--add-modules=javafx.controls,javafx.fxml", \
	"--add-opens", "javafx.graphics/javafx.scene.text=ALL-UNNAMED", \
	"--add-opens", "javafx.graphics/com.sun.javafx.text=ALL-UNNAMED", \
	"--add-exports", "javafx.graphics/com.sun.javafx.geom=ALL-UNNAMED", \
	"--add-exports", "javafx.graphics/com.sun.javafx.text=ALL-UNNAMED", \
	"--add-exports", "javafx.graphics/com.sun.javafx.scene.text=ALL-UNNAMED", \
	"-jar", "cora-0.1.9-SNAPSHOT.jar"]
