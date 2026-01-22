#ifndef CALLBACKSTORE_H_INCLUDED
#define CALLBACKSTORE_H_INCLUDED

namespace Controller
{
    struct CallbackStore
    {
        CallbackStore(void* v = nullptr, void* c = nullptr)
            : view(v), controller(c)
        {
        }
        void* view;
        void* controller;
    };
}

#endif // CALLBACKSTORE_H_INCLUDED