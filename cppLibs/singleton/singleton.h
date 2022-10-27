/*****************************************************************//**
 * @file   test.h
 * @brief  可继承单例类, CRTP, 模板参数为子类
 *
 * @author yd
 * @note  跨库使用的子类必需导出，否则无法保证实例全局唯一
 * @date   October 2022
 *********************************************************************/

#pragma once
template<typename T>
class Singleton {
protected:
	Singleton() = default;
	virtual ~Singleton() = default;
	/**
	 * @brief 代理类，用于禁止用户调用派生类构造函数，保证对象唯一性.
	 */
	struct token {};
private:
	/**
	 * @brief 禁止拷贝构造与拷贝赋值.
	 */
	Singleton(const Singleton&) = delete;
	Singleton& operator=(const Singleton&) = delete;
public:
	/**
	 * @brief 取得唯一实例.
	 */
	static T& GetInstance() {
		static T instance{ token() };
		return instance;
	}
};
