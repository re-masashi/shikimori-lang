#include <chrono>
#include <string>
#include <iostream>
#include <sstream>
#include <filesystem>

#define SHIKIMORI_DEBUG true

#ifdef SHIKIMORI_DEBUG 

inline const char* short_path(const char* path) {
    static std::string cache;
    cache = path;
    auto pos = cache.rfind('/');
    if (pos != std::string::npos) {
        cache = cache.substr(pos + 1);
    }
    return cache.c_str();
}

inline void do_log(const char* file, int line, const char* func, 
                   const std::string& msg) {
    std::cout << "[" << short_path(file) << ":" << line << " in " << func << "] " << msg << std::endl;
}

template<typename... Args>
inline void do_log(const char* file, int line, const char* func, 
                   const std::string& msg, Args&&... args) {
    std::cout << "[" << short_path(file) << ":" << line << " in " << func << "] " << msg << " ";
    ((std::cout << args << " "), ...);
    std::cout << std::endl;
}

inline void do_log(const char* file, int line, const char* func, 
                   const char* msg) {
    std::cout << "[" << short_path(file) << ":" << line << " in " << func << "] " << msg << std::endl;
}

template<typename... Args>
inline void do_log(const char* file, int line, const char* func, 
                   const char* msg, Args&&... args) {
    std::cout << "[" << short_path(file) << ":" << line << " in " << func << "] " << msg << " ";
    ((std::cout << args << " "), ...);
    std::cout << std::endl;
}

#define LOG(...) do_log(__FILE__, __LINE__, __FUNCTION__, __VA_ARGS__)

#else
  #define LOG(...) ((void)0)
#endif // DEBUG_BUILD

#ifdef SHIKIMORI_DEBUG
    class ScopedTracer {
    public:
        ScopedTracer(std::string name) 
            : m_name(std::move(name)), m_start(std::chrono::steady_clock::now()) {}

        ~ScopedTracer() {
            auto end = std::chrono::steady_clock::now();
            auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - m_start);
            std::cout << "[TRACE] " << m_name << " took " << duration.count() << "us" << std::endl;
        }
    private:
        std::string m_name;
        std::chrono::steady_clock::time_point m_start;
    };

    #define TRACE_SCOPE() ScopedTracer tracer_##__LINE__(__FUNCTION__)
#else
    #define TRACE_SCOPE() // Becomes nothing in Release
#endif

template<class... Ts>
struct overload : Ts... {
  using Ts::operator()...;
};

template<class... Ts>
overload(Ts...) -> overload<Ts...>;


