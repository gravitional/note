// For C++ users a RAII safe class, sundials::Context, is provided:

namespace sundials
{

    class Context : public sundials::ConvertibleTo<SUNContext>
    {
    public:
        explicit Context(void *comm = nullptr)
        {
            sunctx_ = std::make_unique<SUNContext>();
            SUNContext_Create(comm, sunctx_.get());
        }

        /* disallow copy, but allow move construction */
        Context(const Context &) = delete;
        Context(Context &&) = default;

        /* disallow copy, but allow move operators */
        Context &operator=(const Context &) = delete;
        Context &operator=(Context &&) = default;

        SUNContext Convert() override
        {
            return *sunctx_.get();
        }
        SUNContext Convert() const override
        {
            return *sunctx_.get();
        }
        operator SUNContext() override
        {
            return *sunctx_.get();
        }
        operator SUNContext() const override
        {
            return *sunctx_.get();
        }

        ~Context()
        {
            if (sunctx_)
                SUNContext_Free(sunctx_.get());
        }

    private:
        std::unique_ptr<SUNContext> sunctx_;
    };

} // namespace sundials