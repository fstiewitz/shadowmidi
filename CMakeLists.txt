cmake_minimum_required(VERSION 3.17)
project(shadowmidi)

set(CMAKE_CXX_STANDARD 17)

FIND_PACKAGE(ALSA REQUIRED)

add_executable(shadowmidi main.cpp)

target_link_libraries(shadowmidi ${ALSA_LIBRARIES})
target_include_directories(shadowmidi PRIVATE ${ALSA_INCLUDE_DIRS} ${CMAKE_SOURCE_DIR}/include)
