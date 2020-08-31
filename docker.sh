docker build --tag matrix_puzzle:1.0 .
docker run --name matrix_puzzle_container matrix_puzzle:1.0
docker rm --force matrix_puzzle_container