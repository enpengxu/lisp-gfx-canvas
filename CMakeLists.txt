cmake_minimum_required(VERSION 2.4)

project(lisp-canvas C)

find_package(glfw3 REQUIRED)
#find_package(pthread REQUIRED)
link_libraries(glfw)


include_directories(${glfw_INCLUDE_DIRS} "${CMAKE_CURRENT_SOURCE_DIR}/deps")

if (MATH_LIBRARY)
    link_libraries("${MATH_LIBRARY}")
endif()

if (MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

if (GLFW_USE_OSMESA)
    add_definitions(-DUSE_NATIVE_OSMESA)
endif()

if (WIN32)
    set(ICON glfw.rc)
elseif (APPLE)
    set(ICON glfw.icns)
    set_source_files_properties(glfw.icns PROPERTIES
        MACOSX_PACKAGE_LOCATION "Resources")
endif()

set(GLAD "${CMAKE_CURRENT_SOURCE_DIR}/deps/glad/glad.h"
         "${CMAKE_CURRENT_SOURCE_DIR}/deps/glad.c")
set(GETOPT "${CMAKE_CURRENT_SOURCE_DIR}/deps/getopt.h"
           "${CMAKE_CURRENT_SOURCE_DIR}/deps/getopt.c")
set(TINYCTHREAD "${CMAKE_CURRENT_SOURCE_DIR}/deps/tinycthread.h"
                "${CMAKE_CURRENT_SOURCE_DIR}/deps/tinycthread.c")  

set(CANVAS_SRC "${CMAKE_CURRENT_SOURCE_DIR}/src/canvas.c")
	      
add_executable(lisp-canvas WIN32 MACOSX_BUNDLE ${CANVAS_SRC} ${ICON} ${GLAD})

target_link_libraries(lisp-canvas "${CMAKE_THREAD_LIBS_INIT}")
if (RT_LIBRARY)
    target_link_libraries(particles "${RT_LIBRARY}")
endif()

set(WINDOWS_BINARIES lisp-canvas)

set_target_properties(${WINDOWS_BINARIES} PROPERTIES
                      FOLDER "bin")

if (MSVC)
    # Tell MSVC to use main instead of WinMain for Windows subsystem executables
    set_target_properties(${WINDOWS_BINARIES} PROPERTIES
                          LINK_FLAGS "/ENTRY:mainCRTStartup")
endif()

if (APPLE)
    set_target_properties(lisp-canvas PROPERTIES MACOSX_BUNDLE_BUNDLE_NAME "lisp-canvas")

    set_target_properties(${WINDOWS_BINARIES} PROPERTIES
                          RESOURCE glfw.icns
                          MACOSX_BUNDLE_SHORT_VERSION_STRING ${GLFW_VERSION}
                          MACOSX_BUNDLE_LONG_VERSION_STRING ${GLFW_VERSION_FULL}
                          MACOSX_BUNDLE_ICON_FILE glfw.icns
                          MACOSX_BUNDLE_INFO_PLIST "${GLFW_SOURCE_DIR}/CMake/MacOSXBundleInfo.plist.in")
endif()

