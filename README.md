# shiny_pop_gen

Population genetics toolbox as a Shiny app.  
This repository contains the source code and a Docker image to run the app in a reproducible environment.

👉 GitHub: [vincentmanz/shiny_pop_gen](https://github.com/vincentmanz/shiny_pop_gen)

---

## 1. Install Docker

- **Linux**: [Docker Engine install docs](https://docs.docker.com/engine/install/)  
- **Windows / macOS**: [Docker Desktop](https://www.docker.com/products/docker-desktop/)

Check your install:

```bash
docker --version
```

---

## 2. Get the image

Clone this repository and build the Docker image:

```bash
git clone https://github.com/vincentmanz/shiny_pop_gen.git
cd shiny_pop_gen
docker build -t pgacmdr:latest .
```

*(Optional)* If the image is published to Docker Hub or another registry, you can pull it directly instead of building:

```bash
docker pull <your-dockerhub-username>/pgacmdr:latest
```

---

## 3. Run the app

Start a container and expose the Shiny server on port `3838`:

```bash
docker run --rm -p 3838:3838 pgacmdr:latest
```

Now open your browser at:

👉 [http://localhost:3838/app](http://localhost:3838/app)

---

## 4. Using your own data

If you want the app to access local data files, mount a folder into the container:

```bash
docker run --rm -p 3838:3838   -v /path/to/your/data:/srv/shiny-server/app/data   pgacmdr:latest
```

Inside the app, your files will appear under `data/`.

---

## 5. Stopping the app

Press **CTRL+C** in the terminal.  
The container will stop and be removed automatically.

---

## 6. Troubleshooting

- **Port in use**: If port 3838 is busy, map to another port (e.g. 4000):

  ```bash
  docker run --rm -p 4000:3838 pgacmdr:latest
  ```
  Then open: [http://localhost:4000/app](http://localhost:4000/app).

- **Permissions**: When mounting local folders, make sure you have read permissions.  
  On Windows, use paths like:

  ```bash
  -v C:\Users\YourName\data:/srv/shiny-server/app/data
  ```

---

## 📖 About

- Author: Vincent Manzanilla, Naffiou Kadiri, Thierry de Meeus  
- License: MIT  
- Repository: [vincentmanz/shiny_pop_gen](https://github.com/vincentmanz/shiny_pop_gen)
- Docker Hub: [vincentmanz/pgacmdr](https://hub.docker.com/r/vincentmanz/pgacmdr)
