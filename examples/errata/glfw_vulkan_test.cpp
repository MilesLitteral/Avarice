#define GLFW_INCLUDE_VULKAN //That way GLFW will include its own definitions and automatically load the Vulkan header with it.
#include <GLFW/glfw3.h>

void run() {
    initWindow();
    initVulkan();
    mainLoop();
    cleanup();
}

private:
    void initWindow() {

    }

glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
window = glfwCreateWindow(800, 600, "Vulkan", nullptr, nullptr);

const uint32_t WIDTH = 800;
const uint32_t HEIGHT = 600;
and replaced the window creation call with

window = glfwCreateWindow(WIDTH, HEIGHT, "Vulkan", nullptr, nullptr);
You should now have a initWindow function that looks like this:

void initWindow() {
    glfwInit();

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);

    window = glfwCreateWindow(WIDTH, HEIGHT, "Vulkan", nullptr, nullptr);
}

void mainLoop() {
    while (!glfwWindowShouldClose(window)) {
        glfwPollEvents();
    }
}

void cleanup() {
    glfwDestroyWindow(window);

    glfwTerminate();
}